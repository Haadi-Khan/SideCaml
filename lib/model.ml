open Transformer
open Moderation

type t =
  | NotInitialized
  | ModerationFailed of string
  | GenerationError of string

let transformer_config = ref None
let ( let* ) = Result.bind

let init () =
  try
    transformer_config := Some (init_transformer ());
    Ok ()
  with _ -> Error (GenerationError "Failed to initialize transformer")

let get_config () =
  match !transformer_config with
  | Some c -> Ok c
  | None -> Error NotInitialized

let rec generate_text_internal config ~max_length ~seed length =
  let generated = generate_text config () seed length in
  let moderation_result = moderate_text ~max_length generated in
  if moderation_result |> is_valid then Ok generated
  else generate_text_internal config ~max_length ~seed length
[@@coverage off]

let generate_text ?(max_length = 1000) ?(seed = "") length =
  let* config = get_config () in
  try generate_text_internal config ~max_length ~seed length
  with _ -> Error (GenerationError "Text generation failed")

let generate_sample () =
  let* config = get_config () in
  try generate_text_internal config ~max_length:1000 ~seed:"" 100
  with _ -> Error (GenerationError "Text generation failed")
[@@coverage off]
