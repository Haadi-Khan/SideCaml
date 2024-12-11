open OUnit2
open Final_project.Transformer
open Final_project.Matrix

let m1 = of_array [| [| 1.; 2. |]; [| 3.; 4. |] |]
let m2 = of_array [| [| 5.; 6. |]; [| 7.; 8. |] |]

let m3 =
  of_array
    [| [| 1.; 2.; 3.; 4. |]; [| 5.; 6.; 7.; 8. |]; [| 9.; 10.; 11.; 12. |] |]

let print_array arr =
  "\n" ^ String.concat ", " @@ Array.to_list @@ Array.map Float.to_string arr

let print_2d_array arr =
  arr |> Array.map print_array |> Array.to_list |> String.concat ""

let print_matrix m = m |> to_array |> print_2d_array

let eq_test expected_arr x _ =
  assert_equal (of_array expected_arr) x ~printer:print_matrix

(* [is_close expected actual]*)
let is_close rel_tol =
  Array.for_all2
    (Array.for_all2 (fun x y -> abs_float (x -. y) /. x <= rel_tol))

let is_close_test expected_arr x rel_tol _ =
  assert_bool
    (Printf.sprintf "Arrays are not close:%s (expected)\n%s (actual)"
       (print_2d_array expected_arr)
       (print_matrix x))
    (is_close rel_tol expected_arr (to_array x))

let test_matrix_dot = eq_test [| [| 19.; 22. |]; [| 43.; 50. |] |] (dot m1 m2)

let test_matrix_dot_single =
  let m1 = of_array [| [| 2. |] |] in
  let m2 = of_array [| [| 3. |] |] in
  eq_test [| [| 6. |] |] (dot m1 m2)

let test_matrix_dot_fail _ =
  assert_raises (Invalid_argument "Incompatible dimensions: 1 != 2") (fun () ->
      dot (of_array [| [| 0.0 |] |]) m1)

let test_matrix_transpose =
  eq_test [| [| 1.; 3. |]; [| 2.; 4. |] |] (transpose m1)

let test_matrix_scale = eq_test [| [| 3.; 6. |]; [| 9.; 12. |] |] (scale m1 3.0)

let test_matrix_map2 =
  eq_test [| [| -4.; -4. |]; [| -4.; -4. |] |] (map2 ( -. ) m1 m2)

let test_matrix_softmax =
  is_close_test
    [|
      [| 0.09003057; 0.24472847; 0.66524096 |];
      [| 0.04201007; 0.1141952; 0.84379473 |];
      [| 1. /. 3.; 1. /. 3.; 1. /. 3. |];
    |]
    (softmax
       (of_array
          [| [| 1.0; 2.0; 3.0 |]; [| 4.0; 5.0; 7.0 |]; [| 0.; 0.; 0. |] |]))
    1e-6

let test_matrix_reshape =
  eq_test
    [| [| 1.; 2.; 3.; 4.; 5.; 6. |]; [| 7.; 8.; 9.; 10.; 11.; 12. |] |]
    (reshape m3 2 6)

let test_matrix_concat_empty = eq_test [| [||] |] (concat [])

let test_matrix_concat =
  eq_test
    [| [| 1.; 2.; 5.; 6.; 1.; 2. |]; [| 3.; 4.; 7.; 8.; 3.; 4. |] |]
    (concat [ m1; m2; m1 ])

let test_matrix_map =
  eq_test [| [| 2.; 3. |]; [| 4.; 5. |] |] (map (( +. ) 1.) m1)

let test_matrix_one_hot =
  eq_test [| [| 0.; 0.; 0.; 0.; 0.; 1.; 0. |] |] (one_hot 5 7)

let test_matrix_get_row _ =
  assert_equal [| 9.; 10.; 11.; 12. |] (get_row m3 2) ~printer:print_array

let test_matrix_sum _ = assert_equal 10. (sum m1) ~printer:string_of_float

let test_matrix_elementwise_mul =
  eq_test [| [| 5.; 12. |]; [| 21.; 32. |] |] (elementwise_mul m1 m2)

let test_matrix_ones =
  eq_test [| [| 1.; 1. |]; [| 1.; 1. |]; [| 1.; 1. |] |] (ones (3, 2))

let print_size (a, b) = Printf.sprintf "(%d, %d)" a b

let test_matrix_random_size _ =
  assert_equal (size (random 3 2)) (3, 2) ~printer:print_size

let test_matrix_random_values _ =
  let m = random 3 2 in
  assert_bool
    (Printf.sprintf "Matrix %s should have all values between -1 and 1"
       (print_matrix m))
    (Array.for_all (Array.for_all (fun x -> -1. <= x && x <= 1.)) (to_array m))

let test_matrix_get _ =
  List.iter
    (fun n ->
      let row = n / 4 in
      let col = n mod 4 in
      assert_equal
        (float_of_int (n + 1))
        (get m3 row col) ~printer:string_of_float
        ~msg:
          (Printf.sprintf "test_matrix_get: n = %d, row = %d, col = %d" n row
             col))
    (List.init 12 Fun.id)

let test_matrix_size_empty _ =
  assert_equal (0, 0) (size (of_array [||])) ~printer:print_size

let () =
  run_test_tt_main
    ("test suite"
    >::: [
           "test_matrix_dot" >:: test_matrix_dot;
           "test_matrix_dot_single" >:: test_matrix_dot_single;
           "test_matrix_dot_fail" >:: test_matrix_dot_fail;
           "test_matrix_transpose" >:: test_matrix_transpose;
           "test_matrix_scale" >:: test_matrix_scale;
           "test_matrix_map2" >:: test_matrix_map2;
           "test_matrix_softmax" >:: test_matrix_softmax;
           "test_matrix_reshape" >:: test_matrix_reshape;
           "test_matrix_concat_empty" >:: test_matrix_concat_empty;
           "test_matrix_concat" >:: test_matrix_concat;
           "test_matrix_map" >:: test_matrix_map;
           "test_matrix_one_hot" >:: test_matrix_one_hot;
           "test_matrix_get_row" >:: test_matrix_get_row;
           "test_matrix_sum" >:: test_matrix_sum;
           "test_matrix_elementwise_mul" >:: test_matrix_elementwise_mul;
           "test_matrix_ones" >:: test_matrix_ones;
           "test_matrix_random_size" >:: test_matrix_random_size;
           "test_matrix_random_values" >:: test_matrix_random_values;
           "test_matrix_get" >:: test_matrix_get;
           "test_matrix_size_empty" >:: test_matrix_size_empty;
         ])
