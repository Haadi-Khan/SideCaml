open OUnit2
open Final_project.Transformer
open Final_project.Matrix

let m1 = of_array [| [| 1.; 2. |]; [| 3.; 4. |] |]
let m2 = of_array [| [| 5.; 6. |]; [| 7.; 8. |] |]

let print m =
  m |> to_array |> Array.map Array.to_list |> Array.to_list
  |> List.map (fun row ->
         "\n" ^ String.concat ", " @@ List.map Float.to_string row)
  |> String.concat ""

let eq_test expected_arr x _ =
  assert_equal (of_array expected_arr) x ~printer:print

(* [is_close expected actual]*)
let is_close rel_tol =
  Array.for_all2
    (Array.for_all2 (fun x y -> abs_float (x -. y) /. x <= rel_tol))

let is_close_test expected_arr x rel_tol _ =
  assert_bool
    (Printf.sprintf "Arrays are not close:%s (expected)\n%s (actual)"
       (print (of_array expected_arr))
       (print x))
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

let test_matrix_apply2 =
  eq_test [| [| -4.; -4. |]; [| -4.; -4. |] |] (apply2 ( -. ) m1 m2)

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

let () =
  run_test_tt_main
    ("test suite"
    >::: [
           "test_matrix_dot" >:: test_matrix_dot;
           "test_matrix_dot_single" >:: test_matrix_dot_single;
           "test_matrix_dot_fail" >:: test_matrix_dot_fail;
           "test_matrix_transpose" >:: test_matrix_transpose;
           "test_matrix_scale" >:: test_matrix_scale;
           "test_matrix_apply2" >:: test_matrix_apply2;
           "test_matrix_softmax" >:: test_matrix_softmax;
         ])
