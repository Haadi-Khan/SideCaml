open Core
open Yojson.Basic.Util

type t = float array array

let dot a b =
  let rows_a = Array.length a in
  let cols_a = Array.length a.(0) in
  let rows_b = Array.length b in
  let cols_b = Array.length b.(0) in
  if cols_a <> rows_b then
    invalid_arg
      (Printf.sprintf "Incompatible dimensions: %d != %d" cols_a rows_b)
    [@coverage off];
  Array.init rows_a ~f:(fun i ->
      Array.init cols_b ~f:(fun j ->
          Array.fold ~init:0.
            ~f:(fun acc k -> acc +. (a.(i).(k) *. b.(k).(j)))
            (Array.init cols_a ~f:(fun x -> x))))

let transpose a =
  let rows = Array.length a in
  let cols = Array.length a.(0) in
  Array.init cols ~f:(fun i -> Array.init rows ~f:(fun j -> a.(j).(i)))

let scale a s = Array.map a ~f:(fun row -> Array.map row ~f:(fun x -> x *. s))

let softmax a =
  Array.map a ~f:(fun row ->
      let max_val =
        Array.fold row ~init:Float.neg_infinity ~f:(fun acc x ->
            Float.max acc x)
      in
      let exp_vals = Array.map row ~f:(fun x -> Float.exp (x -. max_val)) in
      let sum = Array.fold exp_vals ~init:0. ~f:( +. ) in
      Array.map exp_vals ~f:(fun x -> x /. sum))

let reshape a rows cols =
  let flattened = Array.concat_map a ~f:Array.copy in
  Array.init rows ~f:(fun i ->
      Array.init cols ~f:(fun j -> flattened.((i * cols) + j)))

let concat matrices =
  match matrices with
  | [] -> [| [||] |]
  | hd :: _ ->
      let rows = Array.length hd in
      Array.init rows ~f:(fun i ->
          Array.concat (List.map matrices ~f:(fun m -> m.(i))))

let map f matrix = Array.map ~f:(Array.map ~f) matrix
let to_array a = a
let of_array a = a

let one_hot index size =
  let arr = Array.create ~len:size 0. in
  arr.(index) <- 1.;
  [| arr |]

let get_row matrix i = matrix.(i)

let sum matrix =
  Array.fold matrix ~init:0. ~f:(fun acc row ->
      Array.fold row ~init:0. ~f:( +. ) +. acc)

let map2 f m1 m2 = Array.map2_exn ~f:(Array.map2_exn ~f) m1 m2
let elementwise_mul m1 m2 = map2 ( *. ) m1 m2
let ones (rows, cols) = Array.make_matrix ~dimx:rows ~dimy:cols 1.0

let random rows cols =
  Array.init rows ~f:(fun _ ->
      Array.init cols ~f:(fun _ -> Random.float 2. -. 1.))

let get a i j = a.(i).(j)

let size m =
  let rows = Array.length m in
  (rows, if rows = 0 then 0 else Array.length m.(0))
