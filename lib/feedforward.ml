open Core

type t = Matrix.mat

let apply input w1 w2 =
  let intermediate = Matrix.dot input w1 in
  Matrix.relu_in_place intermediate;
  Matrix.dot intermediate w2
[@@inline]

let of_matrix x = x [@@inline]
let to_matrix x = x [@@inline]
