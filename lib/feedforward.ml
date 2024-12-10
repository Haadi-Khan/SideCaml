open Core

type t = Matrix.t

let apply input w1 w2 =
  let intermediate = Matrix.to_array (Matrix.dot input w1) in
  let activated =
    Array.map intermediate ~f:(Array.map ~f:(fun x -> Float.max 0. x))
  in
  Matrix.dot (Matrix.of_array activated) w2

let of_array = Matrix.of_array

let to_array = Matrix.to_array

let of_matrix x = x