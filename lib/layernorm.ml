open Core

type t = Matrix.mat

let apply layer =
  let mean = Matrix.mean layer in
  let variance = Matrix.var ~mean_:mean layer in
  let denominator = Float.sqrt (Matrix.vec_sum variance +. 1e-6) in
  let layer_minus_mean = Matrix.mat_add_vec layer (-1.) mean in
  Matrix.divide_in_place layer_minus_mean denominator;
  layer_minus_mean

let of_matrix x = x [@@inline]
let to_matrix x = x [@@inline]
