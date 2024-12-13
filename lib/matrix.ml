open Core
open Yojson.Basic.Util
open Lacaml.D

type mat = Mat.t
type vec = Vec.t

let size m = (Mat.dim1 m, Mat.dim2 m)
let dot a b = gemm a b [@@inline]

let mat_dot_vec m v =
  let m_rows, m_cols = size m in
  let v_length = Vec.dim v in
  if m_cols <> v_length then
    raise (Failure "Matrix and vector dimensions do not match");
  gemv m v
[@@inline]

let dot_transpose_and_scale a b t = gemm ~transb:`T ~alpha:t a b [@@inline]

let softmax a =
  if Mat.dim1 a = 0 || Mat.dim2 a = 0 then raise (Failure "Matrix is empty");
  let rows = Mat.dim1 a in
  let cols = Mat.dim2 a in
  let expd = Mat.exp a in
  let exp_sums = Mat.fold_cols (fun x y -> Vec.add x y) (Vec.make0 rows) expd in
  Mat.div expd (Mat.of_col_vecs (Array.create ~len:cols exp_sums))

let relu_in_place a = ignore @@ Mat.relu a ~b:a

let reshape a rows cols =
  let arr = Mat.to_array a in
  let old_cols = Mat.dim2 a in
  if Array.length arr * old_cols <> rows * cols then
    raise (Failure "Cannot reshape matrix");
  Mat.init_cols rows cols (fun row col ->
      let i = ((row - 1) * cols) + col - 1 in
      arr.(i / old_cols).(i mod old_cols))

let concat matrices =
  Mat.of_col_vecs @@ Array.concat_map matrices ~f:Mat.to_col_vecs

let map f matrix = Mat.map f matrix
let to_array = Mat.to_array
let of_array = Mat.of_array
let vec_to_array = Vec.to_array
let vec_of_array = Vec.of_array

let one_hot index size =
  if size <= 0 then Mat.of_array [| [||] |]
    (* Return an empty matrix if size is zero *)
  else if index < 0 || index >= size then raise (Failure "Index out of bounds")
  else
    let v = Array.create ~len:size 0. in
    v.(index) <- 1.;
    Mat.of_array [| v |]

let get_row matrix i = Mat.copy_row matrix (i + 1)
let sum matrix = Mat.sum matrix
let elementwise_mul m1 m2 = Mat.mul m1 m2 [@@inline]
let ones (rows, cols) = Mat.make rows cols 1.
let random rows cols = Mat.random rows cols
let get a i j = (Vec.to_array (Mat.to_col_vecs a).(j)).(i)

let add m1 alpha m2 =
  let rows, cols = size m1 in
  let out = Mat.make0 rows cols in
  ignore @@ Mat.add m1 out ~c:out;
  Mat.axpy ~alpha m2 out;
  out

let length = Vec.dim
let to_lacaml_vector = Fun.id
let of_lacaml_vector = Fun.id
let to_lacaml_matrix = Fun.id
let of_lacaml_matrix = Fun.id

let mean m =
  let rows, cols = size m in
  let scale = 1. /. Float.of_int cols in
  let tmp1 = Vec.make0 rows in
  let tmp2 = Vec.make0 rows in
  Mat.fold_cols
    (fun acc v ->
      let v' = copy v ~y:tmp1 in
      scal scale v';
      Vec.add v' acc ~z:tmp2)
    tmp2 m

let var ?mean_ m =
  let mean_ =
    match mean_ with
    | None -> mean m
    | Some x -> x
  in
  let rows, cols = size m in
  let scale = 1. /. Float.of_int (cols - 1) (* Bessel correction *) in

  let tmp1 = Vec.make0 rows in
  let tmp2 = Vec.make0 rows in
  let tmp3 = Vec.make0 rows in
  Mat.fold_cols
    (fun acc v ->
      let sqr_diff = Vec.(sqr (sub mean_ v ~z:tmp1) ~y:tmp2) in
      scal scale sqr_diff;
      Vec.add acc sqr_diff ~z:tmp3)
    tmp3 m

let vec_sum v = Vec.sum v

let mat_add_vec m alpha v =
  Mat.of_col_vecs
  @@ Array.map (Mat.to_col_vecs m) ~f:(fun col ->
         let col' = copy col in
         axpy ~alpha v col';
         col')

let divide_in_place m t = ignore @@ Mat.map (fun x -> x /. t) ~b:m m
let scale m scalar = Mat.map (fun x -> x *. scalar) m
