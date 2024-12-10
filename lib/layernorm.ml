open Core

type t = float array array

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

let of_matrix matrix = Matrix.to_array matrix

let to_matrix layer = Matrix.of_array layer