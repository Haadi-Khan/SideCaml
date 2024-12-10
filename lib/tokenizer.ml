open Core

type t = unit

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