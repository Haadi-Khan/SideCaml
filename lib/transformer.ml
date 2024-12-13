open Core
open Yojson.Basic.Util
open Tokenizer
open Matrix

type t = {
  (* Configuration *)
  vocab_size : int;
  embedding_dim : int;
  num_heads : int;
  num_layers : int;
  dropout : float;
  (* Weights *)
  wk : Matrix.mat; (* Key weights *)
  wq : Matrix.mat; (* Query weights *)
  wv : Matrix.mat; (* Value weights *)
  wo : Matrix.mat; (* Output weights *)
  w1 : Matrix.mat; (* First feedforward layer *)
  w2 : Matrix.mat; (* Second feedforward layer *)
}

let update_weights (model : t) (learning_rate : float) (gradients : Matrix.mat)
    : t =
  let update_matrix m g = Matrix.add m (-.learning_rate) g in
  {
    model with
    wk = update_matrix model.wk gradients;
    wq = update_matrix model.wq gradients;
    wv = update_matrix model.wv gradients;
    wo = update_matrix model.wo gradients;
    w1 = update_matrix model.w1 gradients;
    w2 = update_matrix model.w2 gradients;
  }
[@@coverage off]

type post = {
  text : string;
  vote_total : int;
  comment_count : int;
  created_at : string;
}

let get_text = function
  | { text; _ } -> text
[@@coverage off]

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
[@@coverage off]

let prepare_training_data posts =
  List.map posts ~f:(fun post -> post.text) |> String.concat ~sep:" "
[@@coverage off]

let scaled_dot_product_attention query key value mask =
  let _, key_cols = Matrix.size key in
  let d_k = Float.of_int key_cols in
  let scaled_scores =
    dot_transpose_and_scale query key (1. /. Float.sqrt d_k)
  in
  let masked_scores =
    match mask with
    | Some m -> elementwise_mul scaled_scores m
    | None -> scaled_scores
  in
  let attention_weights = softmax masked_scores in
  dot attention_weights value
[@@coverage off]

let multi_head_attention config query key value mask =
  let head_dim = config.embedding_dim / config.num_heads in
  let split_heads x = reshape x config.num_heads head_dim in
  let q = split_heads query in
  let k = split_heads key in
  let v = split_heads value in
  let sdpa = scaled_dot_product_attention q k v mask in
  let heads = Array.init config.num_heads ~f:(fun _ -> sdpa) in
  concat heads
[@@coverage off]

let feedforward input w1 w2 =
  let intermediate = Matrix.dot input w1 in
  Matrix.relu_in_place intermediate;
  Matrix.dot intermediate w2
[@@coverage off]

let layernorm layer =
  let mean = Matrix.mean layer in
  let variance = Matrix.var ~mean_:mean layer in
  let denominator = Float.sqrt (Matrix.vec_sum variance +. 1e-6) in
  let layer_minus_mean = Matrix.mat_add_vec layer (-1.) mean in
  Matrix.divide_in_place layer_minus_mean denominator;
  layer_minus_mean
[@@coverage off]

let transformer_block config input =
  let attention = multi_head_attention config input input input None in
  let normalized = layernorm attention in
  let w1 = Matrix.random config.embedding_dim (4 * config.embedding_dim) in
  let w2 = Matrix.random (4 * config.embedding_dim) config.embedding_dim in
  let ff = feedforward normalized w1 w2 in
  layernorm ff
[@@coverage off]

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

let sample_with_temperature logits temperature =
  let scaled_logits = Array.map logits ~f:(fun x -> x /. temperature) in
  let probs = softmax (Matrix.of_array [| scaled_logits |]) in
  sample_from_distribution (Matrix.to_array probs).(0)

let beam_search distribution beam_size max_length =
  let best_index = ref 0 in
  let best_prob = ref Float.neg_infinity in
  Array.iteri distribution ~f:(fun i prob ->
      if Float.(prob > !best_prob) then (
        best_prob := prob;
        best_index := i));
  !best_index
[@@coverage off]

let is_repetitive tokens window_size =
  let len = Array.length tokens in
  if len < window_size * 2 then false
  else
    let window1 =
      Array.sub tokens ~pos:(len - (window_size * 2)) ~len:window_size
    in
    let window2 = Array.sub tokens ~pos:(len - window_size) ~len:window_size in
    Array.equal Int.equal window1 window2

let forward_pass config tokens =
  let input_embeddings =
    Matrix.random (Array.length tokens) config.embedding_dim
  in
  let transformer_output = transformer_block config input_embeddings in
  let last_transformer_output =
    Matrix.(get_row transformer_output (fst (size transformer_output) - 1))
  in
  mat_dot_vec
    (Matrix.random config.vocab_size config.embedding_dim)
    last_transformer_output
[@@coverage off]

let generate_text config () start_token length =
  let temperature = 0.7 in
  let tokens = Array.create ~len:(length + 1) 0 in
  let initial = encode start_token in
  Array.blit ~src:initial ~src_pos:0 ~dst:tokens ~dst_pos:0
    ~len:(Array.length initial);
  for pos = 0 to length - 1 do
    let logits = Matrix.vec_to_array @@ forward_pass config tokens in
    let next_token = sample_with_temperature logits temperature in
    tokens.(pos + 1) <- next_token
  done;
  decode tokens
[@@coverage off]

let init_transformer () =
  let embedding_dim = 512 in
  let config =
    {
      vocab_size = 10000;
      embedding_dim;
      num_heads = 8;
      num_layers = 6;
      dropout = 0.1;
      wk = Matrix.random embedding_dim embedding_dim;
      wq = Matrix.random embedding_dim embedding_dim;
      wv = Matrix.random embedding_dim embedding_dim;
      wo = Matrix.random embedding_dim embedding_dim;
      w1 = Matrix.random embedding_dim (4 * embedding_dim);
      w2 = Matrix.random (4 * embedding_dim) embedding_dim;
    }
  in
  let posts = load_posts "data/posts.json" in
  let training_text = prepare_training_data posts in
  let _ = encode training_text in
  config
[@@coverage off]

let load_model filename =
  let ic = In_channel.create filename in
  let model = Marshal.from_channel ic in
  In_channel.close ic;
  model
[@@coverage off]

(* Load pretrained weights and update config *)
let load_pretrained checkpoint_path =
  let model = load_model checkpoint_path in
  {
    model with
    (* Keep original architecture config but load pretrained weights *)
    wk = model.wk;
    wq = model.wq;
    wv = model.wv;
    wo = model.wo;
    w1 = model.w1;
    w2 = model.w2;
  }
[@@coverage off]

(* Initialize with pretrained weights *)
let init_transformer_pretrained checkpoint_path =
  let config = load_pretrained checkpoint_path in
  let posts = load_posts "data/posts.json" in
  let training_text = prepare_training_data posts in
  let _ = encode training_text in
  config
[@@coverage off]

let get_random_first_word text =
  let ic = In_channel.create text in
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
  | words -> List.nth_exn words (Random.int (List.length words))
[@@coverage off]

let clean_text text =
  text |> String.lowercase
  |> String.filter ~f:(fun c ->
         Char.is_alphanum c || Char.is_whitespace c || Char.equal c '.'
         || Char.equal c '!' || Char.equal c '?')

let position_encoding max_len d_model =
  Array.init max_len ~f:(fun pos ->
      Array.init d_model ~f:(fun i ->
          let angle =
            float_of_int pos
            /. (10000. ** (float_of_int (i / 2) /. float_of_int d_model))
          in
          if i mod 2 = 0 then Float.sin angle else Float.cos angle))
  |> Matrix.of_array
[@@coverage off]
