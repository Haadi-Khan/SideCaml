open OUnit2
open Final_project.Transformer

let test_hello _ = assert_equal 2 (1 + 1)

let test_matrix_dot _ =
  let m1 = [| [| 1.; 2. |]; [| 3.; 4. |] |] in
  let m2 = [| [| 5.; 6. |]; [| 7.; 8. |] |] in
  let expected = [| [| 19.; 22. |]; [| 43.; 50. |] |] in
  assert_equal expected (Matrix.dot m1 m2)

let test_matrix_dot_single _ =
  let m1 = [| [| 2. |] |] in
  let m2 = [| [| 3. |] |] in
  let expected = [| [| 6. |] |] in
  assert_equal expected (Matrix.dot m1 m2)

let () =
  run_test_tt_main
    ("test suite"
    >::: [
           "test_hello" >:: test_hello;
           "test_matrix_dot" >:: test_matrix_dot;
           "test_matrix_dot_single" >:: test_matrix_dot_single;
         ])
