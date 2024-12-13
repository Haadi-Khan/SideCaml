type t = {
  is_valid : bool;
  reason : string option;
}

(** Read a list of banned words from data. There's a limited set of bad words
    from training so I based it off of that*)
let banned_words =
  let ic = open_in "../data/badwords.txt" in
  let rec read_lines acc =
    try
      let line = input_line ic |> String.trim in
      if line = "" then read_lines acc else read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      acc
  in
  read_lines []

let check_text_length max_length text =
  if String.length text > max_length then
    {
      is_valid = false;
      reason =
        Some
          (Printf.sprintf "Text exceeds maximum length of %d characters"
             max_length);
    }
  else { is_valid = true; reason = None }

let contains_banned_words text =
  let lower_text = String.lowercase_ascii text in
  let contains_word word =
    let words = String.split_on_char ' ' lower_text in
    List.mem (String.lowercase_ascii word) words
  in
  if List.exists contains_word banned_words then
    { is_valid = false; reason = Some "Text contains inappropriate language" }
  else { is_valid = true; reason = None }

let moderate_text ?(max_length = 1000) text =
  let length_check = check_text_length max_length text in
  if not length_check.is_valid then length_check else contains_banned_words text

let get_failure_reason result =
  match result.reason with
  | Some reason -> reason
  | None -> "Unknown reason"

let is_valid result = result.is_valid
