open Core
open Yojson.Basic.Util

module Matrix = struct
  (** [dot a b] returns [a] times [b]. *)
  let dot a b =
    let rows_a = Array.length a in
    let cols_a = Array.length a.(0) in
    let rows_b = Array.length b in
    let cols_b = Array.length b.(0) in
    if cols_a <> rows_b then
      invalid_arg
        (Printf.sprintf "Incompatible dimensions: %d != %d" cols_a rows_b);
    Array.init rows_a ~f:(fun i ->
        Array.init cols_b ~f:(fun j ->
            Array.fold ~init:0.
              ~f:(fun acc k -> acc +. (a.(i).(k) *. b.(k).(j)))
              (Array.init cols_a ~f:(fun x -> x))))

  (* Transpose matrix *)
  let transpose a =
    let rows = Array.length a in
    let cols = Array.length a.(0) in
    Array.init cols ~f:(fun i -> Array.init rows ~f:(fun j -> a.(j).(i)))

  (* Scale matrix by scalar *)
  let scale a s = Array.map a ~f:(fun row -> Array.map row ~f:(fun x -> x *. s))

  (* Element-wise operation *)
  let apply2 f a b =
    Array.map2_exn a b ~f:(fun row1 row2 -> Array.map2_exn row1 row2 ~f)

  (* Softmax function *)
  let softmax a =
    Array.map a ~f:(fun row ->
        let max_val =
          Array.fold row ~init:Float.neg_infinity ~f:(fun acc x ->
              Float.max acc x)
        in
        let exp_vals = Array.map row ~f:(fun x -> Float.exp (x -. max_val)) in
        let sum = Array.fold exp_vals ~init:0. ~f:( +. ) in
        Array.map exp_vals ~f:(fun x -> x /. sum))

  (* Reshape matrix - simplified version assuming compatible dimensions *)
  let reshape a dims =
    let flattened = Array.concat_map a ~f:Array.copy in
    Array.init dims.(0) ~f:(fun i ->
        Array.init dims.(1) ~f:(fun j -> flattened.((i * dims.(1)) + j)))

  (* Concatenate list of matrices *)
  let concat matrices =
    match matrices with
    | [] -> [| [||] |]
    | hd :: _ ->
        let rows = Array.length hd in
        Array.init rows ~f:(fun i ->
            Array.concat (List.map matrices ~f:(fun m -> m.(i))))
end

(* Types for the input data structure *)
type post = {
  text : string;
  vote_total : int;
  comment_count : int;
  created_at : string;
}

(* JSON loading and parsing *)
let load_posts filename =
  let json = Yojson.Basic.from_file filename in
  json |> to_list
  |> List.map ~f:(fun post ->
         {
           text = post |> member "text" |> to_string;
           vote_total = post |> member "vote_total" |> to_int;
           comment_count = post |> member "comment_count" |> to_int;
           created_at = post |> member "created_at" |> to_string;
         })

(* Prepare training data from posts *)
let prepare_training_data posts =
  List.map posts ~f:(fun post -> post.text) |> String.concat ~sep:" "

(* Transformer parameters *)
type transformer_config = {
  vocab_size : int;
  embedding_dim : int;
  num_heads : int;
  num_layers : int;
  dropout : float;
}

(* Attention mechanism *)
let scaled_dot_product_attention query key value mask =
  let d_k = Float.of_int (Array.length key.(0)) in
  let scores = Matrix.dot query (Matrix.transpose key) in
  let scaled_scores = Matrix.scale scores (1. /. Float.sqrt d_k) in
  let masked_scores =
    match mask with
    | Some m -> Matrix.apply2 ( *. ) scaled_scores m
    | None -> scaled_scores
  in
  let attention_weights = Matrix.softmax masked_scores in
  Matrix.dot attention_weights value

(* Multi-head attention *)
let multi_head_attention config query key value mask =
  let head_dim = config.embedding_dim / config.num_heads in
  (* Split input into multiple heads *)
  let split_heads x = Matrix.reshape x [| config.num_heads; head_dim |] in
  let heads =
    List.init config.num_heads ~f:(fun _ ->
        let q = split_heads query in
        let k = split_heads key in
        let v = split_heads value in
        scaled_dot_product_attention q k v mask)
  in
  (* Combine heads *)
  Matrix.concat heads

(* Layer normalization *)
module Layer_norm = struct
  let apply layer =
    let mean =
      Array.map layer ~f:(fun row ->
          Array.fold row ~init:0. ~f:( +. ) /. Float.of_int (Array.length row))
    in
    let variance =
      Array.map2_exn layer mean ~f:(fun row mu ->
          Array.fold row ~init:0. ~f:(fun acc x -> acc +. ((x -. mu) ** 2.))
          /. Float.of_int (Array.length row))
    in
    Array.map2_exn layer mean ~f:(fun row mu ->
        Array.map row ~f:(fun x ->
            (x -. mu)
            /. Float.sqrt (Array.fold variance ~init:0. ~f:( +. ) +. 1e-6)))
end

(* Feed-forward network *)
module Feed_forward = struct
  let apply input w1 w2 =
    (* First projection: input -> intermediate *)
    let intermediate = Matrix.dot input w1 in
    (* dim: (batch × d_model) × (d_model × d_ff) *)
    let activated =
      Array.map intermediate ~f:(Array.map ~f:(fun x -> Float.max 0. x))
    in
    (* Second projection: intermediate -> output *)
    Matrix.dot activated w2 (* dim: (batch × d_ff) × (d_ff × d_model) *)
end

(* Simple tokenizer *)
module Tokenizer2 = struct
  let vocab = Hashtbl.create (module String)
  let reverse_vocab = Hashtbl.create (module Int)
  let next_id = ref 0

  let encode text =
    String.split text ~on:' '
    |> List.map ~f:(fun word ->
           match Hashtbl.find vocab word with
           | Some id -> id
           | None ->
               let id = !next_id in
               next_id := !next_id + 1;
               Hashtbl.set vocab ~key:word ~data:id;
               Hashtbl.set reverse_vocab ~key:id ~data:word;
               id)
    |> Array.of_list

  let decode tokens =
    Array.map tokens ~f:(fun token ->
        match Hashtbl.find reverse_vocab token with
        | Some word -> word
        | None -> "<UNK>")
    |> Array.to_list |> String.concat ~sep:" "
end

(* Main transformer block *)
let transformer_block config input =
  let attention = multi_head_attention config input input input None in
  let normalized = Layer_norm.apply attention in
  (* Initialize weight matrices with random values *)
  let w1 =
    Array.init config.embedding_dim ~f:(fun _ ->
        Array.init (4 * config.embedding_dim) ~f:(fun _ ->
            Random.float 2. -. 1.))
  in
  let w2 =
    Array.init (4 * config.embedding_dim) ~f:(fun _ ->
        Array.init config.embedding_dim ~f:(fun _ -> Random.float 2. -. 1.))
  in
  let feedforward = Feed_forward.apply normalized w1 w2 in
  Layer_norm.apply feedforward

(* Helper function to prepare input for the transformer *)
let prepare_input tokens =
  let embedding_dim = 512 in
  (* Example dimension *)
  Array.map tokens ~f:(fun token ->
      Array.init embedding_dim ~f:(fun _ -> Random.float 2. -. 1.))

(* Sample from probability distribution *)
let sample_from_distribution probs =
  let cumsum =
    Array.fold probs ~init:[] ~f:(fun acc p ->
        match acc with
        | [] -> [ p ]
        | hd :: _ -> (hd +. p) :: acc)
    |> List.rev
  in
  let r = Random.float 1. in
  let rec find_index cs i =
    match cs with
    | [] -> 0
    | hd :: _ when Float.(r <= hd) -> i
    | _ :: tl -> find_index tl (i + 1)
  in
  find_index cumsum 0

(* Add temperature to control randomness *)
let sample_with_temperature logits temperature =
  let scaled_logits = Array.map logits ~f:(fun x -> x /. temperature) in
  let probs = Matrix.softmax [| scaled_logits |] in
  sample_from_distribution probs.(0)

(* Add beam search for better sequence generation *)
let beam_search logits beam_width max_length =
  let best_index = ref 0 in
  let best_prob = ref Float.neg_infinity in
  Array.iteri logits ~f:(fun i prob ->
      if Float.(prob > !best_prob) then (
        best_prob := prob;
        best_index := i));
  !best_index

(* Add early stopping to prevent repetitive output *)
let is_repetitive tokens window_size =
  let len = Array.length tokens in
  if len < window_size * 2 then false
  else
    let window1 =
      Array.sub tokens ~pos:(len - (window_size * 2)) ~len:window_size
    in
    let window2 = Array.sub tokens ~pos:(len - window_size) ~len:window_size in
    Array.equal Int.equal window1 window2

(* Forward pass through the transformer *)
let forward_pass config tokens =
  let input_embeddings = prepare_input tokens in
  let transformer_output = transformer_block config input_embeddings in
  (* Project to vocabulary size *)
  let logits =
    Matrix.dot transformer_output
      (Array.init config.embedding_dim ~f:(fun _ ->
           Array.init config.vocab_size ~f:(fun _ -> Random.float 2. -. 1.)))
  in
  logits.(Array.length logits - 1)

(* Text generation function *)
let generate_text config () start_token length =
  let temperature = 0.7 in

  (* Create an array with enough space for the full sequence *)
  let tokens = Array.create ~len:(length + 1) 0 in
  let initial = Tokenizer2.encode start_token in

  (* Copy initial tokens *)
  Array.blit ~src:initial ~src_pos:0 ~dst:tokens ~dst_pos:0
    ~len:(Array.length initial);

  (* Generate one token at a time *)
  for pos = 0 to length - 1 do
    let logits = forward_pass config tokens in
    let next_token = sample_with_temperature logits temperature in
    tokens.(pos + 1) <- next_token
  done;

  Tokenizer2.decode tokens

(* Initialize transformer with data *)
let init_transformer () =
  let config =
    {
      vocab_size = 10000;
      embedding_dim = 512;
      num_heads = 8;
      num_layers = 6;
      dropout = 0.1;
    }
  in
  let posts = load_posts "data/posts.json" in
  let training_text = prepare_training_data posts in
  (* Initialize tokenizer with training data *)
  let _ = Tokenizer2.encode training_text in
  config

let get_random_first_word path =
  let ic = In_channel.create path in
  let json = Yojson.Basic.from_channel ic in
  In_channel.close ic;
  let posts = json |> to_list in
  let first_words =
    List.filter_map posts ~f:(fun post ->
        let text = post |> member "text" |> to_string in
        match String.split text ~on:' ' with
        | [] -> None
        | first :: _ -> Some first)
  in
  match first_words with
  | [] -> failwith "No posts found"
  | words -> (
      match List.nth_exn words (Random.int (List.length words)) with
      | word -> word)

(* Main function to generate text *)
let generate_sample () =
  let config = init_transformer () in
  generate_text config () (get_random_first_word "data/posts.json") 10

(* Add text cleaning and normalization *)
let clean_text text =
  text |> String.lowercase
  |> String.filter ~f:(fun c ->
         Char.is_alphanum c || Char.is_whitespace c || Char.equal c '.'
         || Char.equal c '!' || Char.equal c '?')

(* Add sinusoidal position embeddings *)
let position_encoding max_len d_model =
  Array.init max_len ~f:(fun pos ->
      Array.init d_model ~f:(fun i ->
          let angle =
            float_of_int pos
            /. (10000. ** (float_of_int (i / 2) /. float_of_int d_model))
          in
          if i mod 2 = 0 then Float.sin angle else Float.cos angle))
