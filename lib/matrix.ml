open Core
open Yojson.Basic.Util
open Lacaml.D

type t = Mat.t

let dot a b = gemm a b [@@inline]
let transpose a = Mat.transpose_copy a
let scale a s = Mat.map (( *. ) s) a

let softmax a =
  let rows = Mat.dim1 a in
  let cols = Mat.dim2 a in
  let expd = Mat.exp a in
  let exp_sums = Mat.fold_cols (fun x y -> Vec.add x y) (Vec.make0 rows) expd in
  Mat.div expd (Mat.of_col_vecs (Array.create ~len:cols exp_sums))

let reshape a rows cols =
  let arr = Mat.to_array a in
  let old_cols = Mat.dim2 a in
  Mat.init_cols rows cols (fun row col ->
      let i = ((row - 1) * cols) + col - 1 in
      (* Printf.printf "row = %d, col = %d, i = %d, %d, %d\n" row col i (i /
         old_cols) (i mod old_cols); *)
      arr.(i / old_cols).(i mod old_cols))

let concat matrices =
  Mat.of_col_vecs_list @@ List.concat_map matrices ~f:Mat.to_col_vecs_list

let map f matrix = Mat.map f matrix
let to_array = Mat.to_array
let of_array = Mat.of_array

let one_hot index size =
  let v = Array.create ~len:size 0. in
  v.(index) <- 1.;
  Mat.of_array [| v |]

let get_row matrix i = (Mat.to_array matrix).(i)
let sum matrix = Mat.sum matrix

let map2 f m1 m2 =
  Mat.of_array
    (Array.map2_exn ~f:(Array.map2_exn ~f) (Mat.to_array m1) (Mat.to_array m2))

let elementwise_mul m1 m2 = Mat.mul m1 m2
let ones (rows, cols) = Mat.make rows cols 1.
let random rows cols = Mat.random rows cols
let get a i j = (Vec.to_array (Mat.to_col_vecs a).(j)).(i)
let size m = (Mat.dim1 m, Mat.dim2 m)
