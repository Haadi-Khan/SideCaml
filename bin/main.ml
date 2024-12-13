open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Yojson.Safe.Util
open Final_project.Transformer
open Final_project.Pretrain
open Final_project.Model
open ANSITerminal

let url = Uri.of_string "https://api.sidechat.lol/v1/posts/home"

let headers =
  let ic = open_in "data/api.txt" in
  let headers = ref [] in
  (try
      while true do
        let line = input_line ic in
        match String.split_on_char '|' line with
        | [key; value] -> 
            headers := (String.trim key, String.trim value) :: !headers
        | _ -> ()
      done
    with End_of_file -> close_in ic);
  Header.of_list !headers

let initial_payload =
  `Assoc
    [
      ("school_group_id", `String "73466f01-1f5c-4163-b8f7-c96292eeec67");
      ("type", `String "recent");
    ]

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

let clear_screen () =
  ANSITerminal.print_string [] "\027[2J";
  ANSITerminal.print_string [] "\027[H";
  ()

let display_fetch_progress current total =
  clear_screen ();
  ignore (ANSITerminal.printf [ Bold; blue ] "\nFetching Posts Progress:\n\n");
  let width = 50 in
  let progress =
    float_of_int current *. float_of_int width /. float_of_int total
  in
  let filled = int_of_float progress in
  let empty = width - filled in
  ignore
    (Printf.printf "[%s%s] %d/%d\n" (String.make filled '#')
       (String.make empty ' ') current total);
  ()

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

let rec fetch_posts cursor acc count total =
  if count = 0 then Lwt.return acc
  else
    send_request ~cursor () >>= fun response ->
    let cursor, posts = process_response response in
    display_fetch_progress (total - count + 1) total;
    match cursor with
    | Some next_cursor ->
        fetch_posts (Some next_cursor) (acc @ posts) (count - 1) total
    | None -> Lwt.return acc

(** Original post generation functions preserved exactly *)
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

let draw_fancy_box text =
  let width = max 60 (String.length text + 4) in
  let top_border = String.make width '-' in
  let bottom_border = String.make width '-' in
  Printf.printf "+%s+\n" top_border;
  Printf.printf "| %-*s |\n" (width - 2) text;
  Printf.printf "+%s+\n" bottom_border;
  ()

let display_menu () =
  clear_screen ();
  ignore (ANSITerminal.printf [ Bold; blue ] "\nSidechat Post Generator\n\n");
  ignore (ANSITerminal.printf [ white ] "1. Fetch new posts\n");
  ignore (ANSITerminal.printf [ white ] "2. Show random post\n");
  ignore (ANSITerminal.printf [ white ] "3. Generate post (Probabilistic)\n");
  ignore (ANSITerminal.printf [ white ] "4. Generate post (Transformer)\n");
  ignore (ANSITerminal.printf [ white ] "5. Train model\n");
  ignore (ANSITerminal.printf [ white ] "6. Exit\n\n");
  ignore (ANSITerminal.printf [ cyan ] "Enter your choice (1-6): ");
  ()

let display_training_menu () =
  clear_screen ();
  ignore (ANSITerminal.printf [ Bold; blue ] "\nTraining Options\n\n");
  ignore (ANSITerminal.printf [ white ] "1. Pretrain on wiki\n");
  ignore (ANSITerminal.printf [ white ] "2. Fine-tune pretrained model\n");
  ignore (ANSITerminal.printf [ white ] "3. Return to main menu\n\n");
  ignore (ANSITerminal.printf [ cyan ] "Enter your choice (1-3): ");
  ()

let display_error msg =
  ignore (ANSITerminal.printf [ Bold; red ] "\nError: %s\n" msg);
  ignore (Unix.sleep 2);
  ()

let display_success msg =
  ignore (ANSITerminal.printf [ Bold; green ] "\nSuccess: %s\n" msg);
  ignore (Unix.sleep 2);
  ()

(** Main Program Logic *)
let rec main_loop () =
  display_menu ();
  match read_line () with
  | "1" ->
      clear_screen ();
      ignore
        (ANSITerminal.printf [ cyan ]
           "How many requests? (Each yields ~20 posts): ");
      (match read_int_opt () with
      | None -> display_error "Invalid input"
      | Some req_num when req_num <= 0 ->
          display_error "Number must be positive"
      | Some req_num ->
          let result = Lwt_main.run (fetch_posts None [] req_num req_num) in
          let json_str = `List result |> Yojson.Safe.pretty_to_string in
          let oc = open_out "posts.json" in
          output_string oc json_str;
          close_out oc;
          let csv = json_to_csv (`List result) in
          let oc = open_out "posts.csv" in
          output_string oc csv;
          close_out oc;
          display_success "Posts saved to posts.json and posts.csv");
      main_loop ()
  | "2" ->
      (try
         let ic = open_in "data/posts.json" in
         let json = Yojson.Safe.from_channel ic in
         close_in ic;
         let posts = json |> to_list in
         let random_post = List.nth posts (Random.int (List.length posts)) in
         let text = random_post |> member "text" |> to_string in
         clear_screen ();
         ignore (ANSITerminal.printf [ Bold; blue ] "\nRandom Post:\n\n");
         draw_fancy_box text;
         ignore (ANSITerminal.printf [ cyan ] "\nPress Enter to continue...");
         ignore (read_line ())
       with _ -> display_error "Could not read posts file");
      main_loop ()
  | "3" ->
      (try
         let generated = generate_post_probabilistically "data/posts.json" in
         clear_screen ();
         ignore (ANSITerminal.printf [ Bold; blue ] "\nGenerated Post:\n\n");
         draw_fancy_box generated;
         ignore (ANSITerminal.printf [ cyan ] "\nPress Enter to continue...");
         ignore (read_line ())
       with _ -> display_error "Could not generate post");
      main_loop ()
  | "4" ->
      (init () |> function
       | Error _ ->
           Printf.printf "Failed to initialize transformer\n";
           main_loop ()
       | Ok () -> (
           let sample = generate_sample () in
           match sample with
           | Ok text ->
               clear_screen ();
               ignore
                 (ANSITerminal.printf [ Bold; blue ]
                    "\nTransformer Generated Post:\n\n");
               draw_fancy_box text;
               ignore
                 (ANSITerminal.printf [ cyan ] "\nPress Enter to continue...");
               ignore (read_line ())
           | Error _ -> display_error "Failed to generate post"));
      main_loop ()
  | "5" ->
      let rec training_loop () =
        display_training_menu ();
        match read_line () with
        | "1" ->
            let config = init_transformer () in
            let tc = training_config 32 0.0001 100 "checkpoints" in
            (try
               let dataset = load_dataset "data/wiki.train.tokens" in
               ignore
                 (ANSITerminal.printf [ Bold; green ]
                    "\nDataset loaded successfully!\n");
               train config tc dataset;
               display_success "Training complete"
             with Sys_error _ ->
               display_error "Path not found: data/wiki.train.tokens");
            training_loop ()
        | "2" ->
            (try
               let config = load_model "checkpoints/model_final.ckpt" in
               let tc = training_config 16 0.00001 10 "checkpoints_finetuned" in
               let dataset = load_dataset "data/posts.json" in
               ignore
                 (ANSITerminal.printf [ Bold; green ]
                    "\nDataset loaded successfully!\n");
               train config tc dataset;
               display_success "Fine-tuning complete"
             with Sys_error _ -> display_error "Model checkpoint not found");
            training_loop ()
        | "3" -> main_loop ()
        | _ ->
            display_error "Invalid choice";
            training_loop ()
      in
      training_loop ()
  | "6" -> exit 0
  | _ ->
      display_error "Invalid choice";
      main_loop ()

let () =
  Random.self_init ();
  clear_screen ();
  ignore
    (ANSITerminal.printf [ Bold; blue ]
       "\nWelcome to Sidechat Post Generator!\n");
  ignore
    (ANSITerminal.printf [ white ]
       "\n\
        This program can fetch posts from Sidechat API and save them to JSON \
        and CSV files.\n");
  ignore
    (ANSITerminal.printf [ white ]
       "An example json file is already in data/posts.json.\n");
  ignore
    (ANSITerminal.printf [ white ]
       "New fetched posts will be written to posts.csv and posts.json in the \
        current directory.\n\n");
  ignore (ANSITerminal.printf [ cyan ] "Press Enter to continue...");
  ignore (read_line ());
  main_loop ()
