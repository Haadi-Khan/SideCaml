open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Yojson.Safe.Util
open Final_project.Transformer
open Tokenizer
open Final_project.Pretrain
open Final_project.Model

let url = Uri.of_string "https://api.sidechat.lol/v1/posts/home"

let headers =
  Header.of_list
    [
      ("accept", "/");
      ("content-type", "application/json");
      ( "authorization",
        "bearer \
         eyJhbGciOiJIUzI1NiJ9.OTA3NjRhMDEtZThhYy00NjJlLWE1OWItZmJmMTQ4ZWM1YmZk.2rFdh885edRFF06kRuqsMNo6ms4i743FMRwWuztg32M"
      );
      ("app-version", "5.4.15");
      ( "accept-language",
        "en-US;q=1.0, ar-US;q=0.9, zh-Hans-US;q=0.8, es-US;q=0.7, ja-US;q=0.6"
      );
      ( "user-agent",
        "sidechat/5.4.15 (com.flowerave.sidechat; build:2; iOS 18.0.1) \
         Alamofire/5.9.1" );
    ]

let initial_payload =
  `Assoc
    [
      ("school_group_id", `String "73466f01-1f5c-4163-b8f7-c96292eeec67");
      ("type", `String "recent");
    ]

(* Function to send a request with an optional cursor field *)
let send_request ?(cursor = None) () =
  let payload =
    match cursor with
    | Some c -> `Assoc (("cursor", `String c) :: to_assoc initial_payload)
    | None -> initial_payload
  in
  let body = Cohttp_lwt.Body.of_string (Yojson.Safe.to_string payload) in
  Client.post ~headers ~body url >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body >|= fun body_string ->
  Yojson.Safe.from_string body_string

(* Process response and filter posts by criteria *)
let process_response json =
  let cursor = json |> member "cursor" |> to_string_option in
  let feed_items = json |> member "feed_items" |> to_list in
  let posts =
    List.fold_left
      (fun acc item ->
        let item_type = item |> member "item_type" |> to_string_option in
        let post = item |> member "post" in
        let assets = post |> member "assets" |> to_list in
        let has_quote_post =
          post |> member "quote_post"
          |> to_option (fun _ -> true)
          |> Option.value ~default:false
        in
        if item_type = Some "post" && assets = [] && not has_quote_post then
          let text =
            post |> member "text" |> to_string_option
            |> Option.value ~default:""
          in
          let vote_total = post |> member "vote_total" |> to_int in
          let comment_count = post |> member "comment_count" |> to_int in
          let created_at =
            post |> member "created_at" |> to_string_option
            |> Option.value ~default:""
          in
          `Assoc
            [
              ("text", `String text);
              ("vote_total", `Int vote_total);
              ("comment_count", `Int comment_count);
              ("created_at", `String created_at);
            ]
          :: acc
        else acc)
      [] feed_items
  in
  (cursor, List.rev posts)
(* Reverse posts to make them most recent to least recent *)

(* Display a progress bar *)
let display_progress current total =
  let percent = current * 100 / total in
  Printf.printf "\rProgress: [%3d%%] (%d/%d requests)" percent current total;
  flush stdout

(* Main loop to fetch data with cursor chaining and accumulate responses *)
let rec fetch_posts cursor acc count total =
  if count = 0 then Lwt.return acc
  else
    send_request ~cursor () >>= fun response ->
    let cursor, posts = process_response response in
    display_progress (total - count + 1) total;
    match cursor with
    | Some next_cursor ->
        fetch_posts (Some next_cursor) (acc @ posts) (count - 1) total
    | None -> Lwt.return acc

let json_to_csv json =
  let posts = json |> to_list in
  let csv_header = "text,vote_total,comment_count,created_at\n" in
  let csv_rows =
    posts
    |> List.map (fun post ->
           let text = post |> member "text" |> to_string in
           let vote_total = post |> member "vote_total" |> to_int in
           let comment_count = post |> member "comment_count" |> to_int in
           let created_at = post |> member "created_at" |> to_string in
           Printf.sprintf "\"%s\",%d,%d,\"%s\"" text vote_total comment_count
             created_at)
  in
  csv_header ^ String.concat "\n" csv_rows

(* Fetch posts and save results to JSON *)
let fetch_posts req_num =
  let result =
    Lwt_main.run (fetch_posts None [] req_num req_num) |> fun posts ->
    `List posts |> Yojson.Safe.pretty_to_string
  in
  let oc = open_out "posts.json" in
  output_string oc result;
  close_out oc;
  let json = Yojson.Safe.from_string result in
  let csv = json_to_csv json in
  let oc = open_out "posts.csv" in
  output_string oc csv;
  close_out oc;
  Printf.printf "\nResults saved to posts.csv\n"

(* Randomly select a post from the data stored in data/posts.json *)
let select_random_post path () =
  let ic = open_in path in
  let json = Yojson.Safe.from_channel ic in
  close_in ic;
  let posts = json |> to_list in
  let random_post = List.nth posts (Random.int (List.length posts)) in
  let text = random_post |> member "text" |> to_string in
  Printf.printf "Here's a random post: %s\n" text

(* Function to map each word to the 10 most common next words along with the
   number of occurrences *)
(* Function to map each word to the 10 most common next words along with the
   number of occurrences, weighted by upvotes *)
let map_word_to_next_words path =
  let ic = open_in path in
  let json = Yojson.Safe.from_channel ic in
  close_in ic;
  let posts = json |> to_list in
  let word_map = Hashtbl.create 1000 in
  let update_word_map word next_word vote_total =
    let next_words =
      match Hashtbl.find_opt word_map word with
      | Some nw -> nw
      | None -> Hashtbl.create 10
    in
    let weight = max 1 vote_total in
    (* Use vote_total as weight, minimum 1 *)
    let count =
      match Hashtbl.find_opt next_words next_word with
      | Some c -> c + weight
      | None -> weight
    in
    Hashtbl.replace next_words next_word count;
    Hashtbl.replace word_map word next_words
  in
  List.iter
    (fun post ->
      let text = post |> member "text" |> to_string in
      let vote_total = post |> member "vote_total" |> to_int in
      let words = String.split_on_char ' ' text in
      List.iter2
        (fun word next_word -> update_word_map word next_word vote_total)
        words
        (List.tl words @ [ "<END>" ]))
    posts;
  let word_to_top_next_words =
    Hashtbl.fold
      (fun word next_words acc ->
        let sorted_next_words =
          Hashtbl.fold
            (fun next_word count acc -> (next_word, count) :: acc)
            next_words []
          |> List.sort (fun (_, c1) (_, c2) -> compare c2 c1)
          |> fun lst ->
          List.fold_left
            (fun (acc, n) x -> if n < 10 then (x :: acc, n + 1) else (acc, n))
            ([], 0) lst
          |> fst
        in
        (word, sorted_next_words) :: acc)
      word_map []
  in
  word_to_top_next_words

let get_random_first_word path =
  let ic = open_in path in
  let json = Yojson.Safe.from_channel ic in
  close_in ic;
  let posts = json |> to_list in
  let first_words =
    List.filter_map
      (fun post ->
        let text = post |> member "text" |> to_string in
        match String.split_on_char ' ' text with
        | first :: _ -> Some first
        | [] -> None)
      posts
  in
  match first_words with
  | [] -> failwith "No posts found"
  | words -> List.nth words (Random.int (List.length words))

(** Generate a new post using [map_word_to_next_words]. *)
let generate_post_probabilistically path =
  let word_to_top_next_words = map_word_to_next_words path in
  let rec generate_words word acc count =
    if
      (count = 0 && List.length acc >= 5)
      || (word = "<END>" && List.length acc >= 5)
    then List.rev acc
    else
      let next_word =
        match List.assoc_opt word word_to_top_next_words with
        | Some next_words ->
            let filtered_words =
              if List.length acc < 5 then
                List.filter (fun (w, _) -> w <> "<END>") next_words
              else next_words
            in
            let next_words =
              if List.length filtered_words = 0 then next_words
              else filtered_words
            in
            let total =
              List.fold_left (fun acc (_, count) -> acc + count) 0 next_words
            in
            let rand = Random.int total in
            let rec pick_word lst acc =
              match lst with
              | (next_word, count) :: tl ->
                  if rand < acc + count then next_word
                  else pick_word tl (acc + count)
              | [] -> "<END>"
            in
            pick_word next_words 0
        | None -> "<END>"
      in
      generate_words next_word (word :: acc) (count - 1)
  in
  let start_word = get_random_first_word path in
  let post_words = generate_words start_word [] 20 in
  String.concat " " post_words

(* Run the function and save results to JSON *)
let () =
  Random.self_init ();
  Printf.printf
    "This program fetches posts from Sidechat API and saves them to a JSON \
     file and a CSV file.\n\
     An example json file is already in data/posts.json.\n\
     The fetched post data are written to posts.csv and posts.json.\n\n\
     Do you want to fetch new posts (y/N) or use the existing data/posts.json? ";
  match read_line () with
  | "y" | "Y" -> (
      Printf.printf
        "How many requests do you want to make? Each request yields ~20 posts. ";
      match read_int_opt () with
      | None -> print_endline "Failed to parse input. Exiting"
      | Some req_num -> (
          fetch_posts req_num;
          Printf.printf
            "\n\
             Do you want to (1) see a random post, (2) tokenize a random post, \
             (3) generate a post, (4) generate using transformer, or (5) \
             pretrain the model? ";
          match read_int_opt () with
          | Some 1 -> select_random_post "posts.json" ()
          | Some 2 ->
              let tokenizer = Tokenizer.load_and_train_tokenizer "posts.json" in
              let text, tokens =
                Tokenizer.tokenize_random_post tokenizer "posts.json"
              in
              Printf.printf "\nOriginal post:\n%s\n\n" text;
              Printf.printf "Tokens:\n[";
              List.iter (fun token -> Printf.printf "%d; " token) tokens;
              Printf.printf "]\n\n";
              let decoded = Tokenizer.decode tokenizer tokens in
              Printf.printf "Decoded back to text:\n%s\n" decoded
          | Some 3 ->
              let generated = generate_post_probabilistically "posts.json" in
              Printf.printf "\nGenerated post:\n%s\n" generated
          | Some 4 -> (
              init () |> function
              | Error _ -> Printf.printf "Failed to initialize transformer\n"
              | Ok () -> (
                  let sample = generate_sample () in
                  match sample with
                  | Ok text ->
                      Printf.printf "\nTransformer generated post:\n%s\n" text
                  | Error _ -> Printf.printf "\nFailed to generate post\n"))
          | Some 5 -> (
              Printf.printf
                "Choose: (1) Pretrain on wiki, (2) Fine-tune pretrained model \
                 on posts? ";
              match read_int_opt () with
              | Some 1 ->
                  let config = init_transformer () in
                  let tc = training_config 32 0.0001 100 "checkpoints" in
                  let dataset =
                    try load_dataset "data/wiki.train.tokens"
                    with Sys_error _ ->
                      Printf.printf "Path not found: data/wiki.train.tokens\n";
                      exit 1
                  in
                  print_endline "Done reading dataset";
                  train config tc dataset
              | Some 2 -> (
                  try
                    let config = load_model "checkpoints/model_final.ckpt" in
                    let tc =
                      training_config 16 0.00001 10 "checkpoints_finetuned"
                    in
                    let dataset = load_dataset "data/posts.json" in
                    print_endline "Done reading dataset";
                    train config tc dataset;
                    print_endline "Fine-tuning complete"
                  with Sys_error _ ->
                    print_endline "Model checkpoint not found")
              | _ -> print_endline "Invalid choice")
          | _ ->
              Printf.printf "Invalid choice, showing random post:\n";
              select_random_post "posts.json" ()))
  | _ -> (
      Printf.printf "No new posts fetched (using data/posts.json).\n";
      Printf.printf
        "Do you want to (1) see a random post, (2) tokenize a random post, (3) \
         generate a post, (4) generate using transformer, or (5) pretrain the \
         model? ";
      match read_int_opt () with
      | Some 1 -> select_random_post "data/posts.json" ()
      | Some 2 ->
          let tokenizer =
            Tokenizer.load_and_train_tokenizer "data/posts.json"
          in
          let text, tokens =
            Tokenizer.tokenize_random_post tokenizer "data/posts.json"
          in
          Printf.printf "\nOriginal post:\n%s\n\n" text;
          Printf.printf "Tokens:\n[";
          List.iter (fun token -> Printf.printf "%d; " token) tokens;
          Printf.printf "]\n\n";
          let decoded = Tokenizer.decode tokenizer tokens in
          Printf.printf "Decoded back to text:\n%s\n" decoded
      | Some 3 ->
          let generated = generate_post_probabilistically "data/posts.json" in
          Printf.printf "\nGenerated post:\n%s\n" generated
      | Some 4 -> (
          init () |> function
          | Error _ -> Printf.printf "Failed to initialize transformer\n"
          | Ok () -> (
              let sample = generate_sample () in
              match sample with
              | Ok text ->
                  Printf.printf "\nTransformer generated post:\n%s\n" text
              | Error _ -> Printf.printf "\nFailed to generate post\n"))
      | Some 5 -> (
          Printf.printf
            "Choose: (1) Pretrain on wiki, (2) Fine-tune pretrained model on \
             posts? ";
          match read_int_opt () with
          | Some 1 ->
              let config = init_transformer () in
              let tc = training_config 32 0.0001 100 "checkpoints" in
              let dataset =
                try load_dataset "data/wikitext/wikitext-103/wiki.train.tokens"
                with Sys_error _ ->
                  Printf.printf
                    "Path not found: \
                     data/wikitext/wikitext-103/wiki.train.tokens\n";
                  exit 1
              in
              print_endline "Done reading dataset";
              train config tc dataset
          | Some 2 -> (
              try
                let config = load_model "checkpoints/model_final.ckpt" in
                let tc =
                  training_config 16 0.00001 10 "checkpoints_finetuned"
                in
                let dataset = load_dataset "data/posts.json" in
                print_endline "Done reading dataset";
                train config tc dataset;
                print_endline "Fine-tuning complete"
              with Sys_error _ -> print_endline "Invalid choice")
          | _ -> print_endline "Invalid choice")
      | _ ->
          Printf.printf "Invalid choice, showing random post:\n";
          select_random_post "data/posts.json" ())
