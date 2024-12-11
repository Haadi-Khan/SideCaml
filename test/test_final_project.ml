open OUnit2
open Final_project.Transformer
open Final_project.Matrix


let test_matrix_dot _ =
  let m1 = of_array [| [| 1.; 2. |]; [| 3.; 4. |] |] in
  let m2 = of_array [| [| 5.; 6. |]; [| 7.; 8. |] |] in
  let expected = of_array [| [| 19.; 22. |]; [| 43.; 50. |] |] in
  assert_equal expected (dot m1 m2)

let test_matrix_dot_single _ =
  let m1 = of_array [| [| 2. |] |] in
  let m2 = of_array [| [| 3. |] |] in
  let expected = of_array [| [| 6. |] |] in
  assert_equal expected (dot m1 m2)

let () =
  run_test_tt_main
    ("test suite"
    >::: [
           "test_matrix_dot" >:: test_matrix_dot;
           "test_matrix_dot_single" >:: test_matrix_dot_single;
         ])
