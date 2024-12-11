open OUnit2
open Final_project.Transformer
open Final_project.Matrix

let m1 = of_array [| [| 1.; 2. |]; [| 3.; 4. |] |]
let m2 = of_array [| [| 5.; 6. |]; [| 7.; 8. |] |]

let print m =
  m 
  |> to_array 
  |> Array.map Array.to_list 
  |> Array.to_list 
  |> List.map (fun row -> "\n" ^ String.concat ", " @@ List.map Float.to_string row)
  |> String.concat ""

  let eq_test expected_arr x =
  fun _ -> assert_equal (of_array expected_arr) x ~printer:print

let test_matrix_dot =
  eq_test [| [| 19.; 22. |]; [| 43.; 50. |] |] (dot m1 m2)

let test_matrix_dot_single =
  let m1 = of_array [| [| 2. |] |] in
  let m2 = of_array [| [| 3. |] |] in
  eq_test [| [| 6. |] |] (dot m1 m2)

let test_matrix_transpose =
  eq_test [| [| 1.; 3. |]; [| 2.; 4.; |] |] (transpose m1)

let () =
  run_test_tt_main
    ("test suite"
    >::: [
           "test_matrix_dot" >:: test_matrix_dot;
           "test_matrix_dot_single" >:: test_matrix_dot_single;
           "test_matrix_transpose" >:: test_matrix_transpose
         ])
