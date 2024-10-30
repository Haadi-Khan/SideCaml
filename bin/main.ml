open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Yojson.Safe.Util

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

(* Run the function and save results to JSON *)
let () =
  let total_requests = 5000 in
  let result =
    Lwt_main.run (fetch_posts None [] total_requests total_requests)
    |> fun posts -> `List posts |> Yojson.Safe.pretty_to_string
  in
  let oc = open_out "posts.json" in
  output_string oc result;
  close_out oc;
  Printf.printf "\nDone! Results saved to posts.json\n"