open OUnit2
open Final_project.Transformer
open Final_project.Matrix
open Final_project.Util
open Final_project.Moderation
open Final_project.Tokenizer
open Final_project.Model
open Final_project.Pretrain

(** Test matrices for Matrix module tests *)
let m1 = of_array [| [| 1.; 2. |]; [| 3.; 4. |] |]

let m2 = of_array [| [| 5.; 6. |]; [| 7.; 8. |] |]

let m3 =
  of_array
    [| [| 1.; 2.; 3.; 4. |]; [| 5.; 6.; 7.; 8. |]; [| 9.; 10.; 11.; 12. |] |]

let m4 =
  of_array
    [|
      [| 1.; 2.; 3.; 4. |];
      [| 2.; 4.; 6.; 7. |];
      [| 8.; 1.; -3.; -33. |];
      [| 0.; 0.; 0.; 0. |];
    |]

let v1 = vec_of_array [| 3.; 5.; 7.; 9. |]

(** Helper functions for testing *)
let print_array arr =
  "\n" ^ String.concat ", " @@ Array.to_list @@ Array.map Float.to_string arr

let print_2d_array arr =
  arr |> Array.map print_array |> Array.to_list |> String.concat ""

let print_vector m = m |> vec_to_array |> print_array
let print_matrix m = m |> to_array |> print_2d_array

let eq_test expected_arr x _ =
  assert_equal expected_arr (to_array x) ~printer:print_2d_array

let eq_test' expected_arr x _ =
  assert_equal expected_arr (vec_to_array x) ~printer:print_array

let is_close' rel_tol =
  Array.for_all2 (fun x y ->
      (abs_float (x -. y) /. if x = 0. then 1. else x) <= rel_tol)

let is_close rel_tol = Array.for_all2 (is_close' rel_tol)

let is_close_test' expected_arr x rel_tol _ =
  assert_bool
    (Printf.sprintf "Arrays are not close:%s (expected)\n%s (actual)"
       (print_array expected_arr) (print_vector x))
    (is_close' rel_tol expected_arr (vec_to_array x))

let is_close_test expected_arr x rel_tol _ =
  assert_bool
    (Printf.sprintf "Arrays are not close:%s (expected)\n%s (actual)"
       (print_2d_array expected_arr)
       (print_matrix x))
    (is_close rel_tol expected_arr (to_array x))

(** Note: fine_tune.mli is intentionally not tested here training neural
    networks is non-deterministic and the training process is computationally
    expensive. Therefore the [@@coverage off] annotation has been applied to
    this module. *)

(** Tests for Matrix module (matrix.mli) *)

let test_softmax_empty _ =
  assert_raises (Failure "Matrix is empty") (fun () -> softmax (of_array [||]))

let test_matrix_dot_identity _ =
  let m = of_array [| [| 1.; 2. |]; [| 3.; 4. |] |] in
  let identity = of_array [| [| 1.; 0. |]; [| 0.; 1. |] |] in
  eq_test (to_array m) (dot m identity)

let test_softmax_single_value _ =
  let m = of_array [| [| 5.0 |] |] in
  let result = softmax m in
  assert_equal (to_array result) [| [| 1.0 |] |]

let test_relu_in_place_empty _ =
  let m = of_array [||] in
  relu_in_place m;
  assert_equal [||] (to_array m)

let test_relu_in_place_all_negative _ =
  let m = of_array [| [| -1.0; -2.0 |]; [| -3.0; -4.0 |] |] in
  relu_in_place m;
  assert_equal [| [| 0.0; 0.0 |]; [| 0.0; 0.0 |] |] (to_array m)

let test_concat_empty_matrices _ = assert_equal (of_array [||]) (concat [||])

let test_concat_single_matrix _ =
  let m = of_array [| [| 1.0; 2.0 |] |] in
  assert_equal m (concat [| m |])

let test_add_with_zero_matrix _ =
  let m1 = of_array [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
  let zero_matrix = of_array [| [| 0.0; 0.0 |]; [| 0.0; 0.0 |] |] in
  assert_equal m1 (add m1 1. zero_matrix)

let test_elementwise_mul_with_zero_matrix _ =
  let m1 = of_array [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |] in
  let zero_matrix = of_array [| [| 0.0; 0.0 |]; [| 0.0; 0.0 |] |] in
  assert_equal zero_matrix (elementwise_mul m1 zero_matrix)

let test_matrix_dot = eq_test [| [| 19.; 22. |]; [| 43.; 50. |] |] (dot m1 m2)

let test_matrix_dot_single =
  let m1 = of_array [| [| 2. |] |] in
  let m2 = of_array [| [| 3. |] |] in
  eq_test [| [| 6. |] |] (dot m1 m2)

let test_matrix_dot_fail _ =
  assert_raises
    (Failure "Lacaml.D.gemm: inner dimensions of matrices do not match (1,2)")
    (fun () -> dot (of_array [| [| 0.0 |] |]) m1)

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

let test_matrix_concat_empty = eq_test [||] (concat [||])

let test_matrix_concat =
  eq_test
    [| [| 1.; 2.; 5.; 6.; 1.; 2. |]; [| 3.; 4.; 7.; 8.; 3.; 4. |] |]
    (concat [| m1; m2; m1 |])

let test_matrix_map =
  eq_test [| [| 2.; 3. |]; [| 4.; 5. |] |] (map (( +. ) 1.) m1)

let test_matrix_one_hot =
  eq_test [| [| 0.; 0.; 0.; 0.; 0.; 1.; 0. |] |] (one_hot 5 7)

let test_matrix_get_row = eq_test' [| 9.; 10.; 11.; 12. |] (get_row m3 2)
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

let test_matrix_mean = eq_test' [| 2.5; 6.5; 10.5 |] (mean m3)

let test_matrix_var_1 =
  is_close_test' [| 5. /. 3.; 59. /. 12.; 3923. /. 12.; 0. |] (var m4) 1e-6

let test_matrix_var_2 =
  is_close_test'
    [| 5. /. 3.; 59. /. 12.; 3923. /. 12.; 0. |]
    (var ~mean_:(mean m4) m4)
    1e-6

let test_matrix_vec_sum _ = assert_equal 24. (vec_sum v1)

let test_matrix_add_vec =
  eq_test
    [|
      [| -5.; -4.; -3. |];
      [| -6.; -5.; -4. |];
      [| -7.; -6.; -5. |];
      [| -8.; -7.; -6. |];
    |]
    (mat_add_vec (reshape m3 4 3) (-2.) v1)

let test_matrix_divide_in_place =
  eq_test
    [|
      [| -3.5; -2.; -4.5; 16. |];
      [| 0.; -3.; -3.5; 1. |];
      [| -4.5; 7.; -5.5; -6. |];
    |]
    (let m =
       of_array
         [|
           [| 7.; 4.; 9.; -32. |];
           [| 0.; 6.; 7.; -2. |];
           [| 9.; -14.; 11.; 12. |];
         |]
     in
     divide_in_place m (-2.);
     m)

let test_dot_transpose_and_scale =
  eq_test
    [| [| 10.; 22. |]; [| 22.; 50. |] |]
    (dot_transpose_and_scale m1 m1 2.)

let test_mat_dot_vec =
  eq_test' [| 11.; 25. |] (mat_dot_vec m1 (vec_of_array [| 3.; 4. |]))

let test_relu_in_place =
  eq_test
    [| [| 1.; 2.; 0. |]; [| 3.; 0.; 4. |] |]
    (let m = of_array [| [| 1.; 2.; -1. |]; [| 3.; -2.; 4. |] |] in
     relu_in_place m;
     m)

let test_add = eq_test [| [| 11.; 14. |]; [| 17.; 20. |] |] (add m1 2. m2)
let test_scale = eq_test [| [| 2.; 4. |]; [| 6.; 8. |] |] (scale m1 2.)
let test_length _ = assert_equal 4 (length v1) ~printer:string_of_int

let test_lacaml_matrix_conversion =
  eq_test
    [| [| 1.; 2. |]; [| 3.; 4. |] |]
    (m1 |> to_lacaml_matrix |> of_lacaml_matrix)

let test_lacaml_vector_conversion =
  eq_test' [| 3.; 5.; 7.; 9. |] (v1 |> to_lacaml_vector |> of_lacaml_vector)

(** Tests for Util module (util.mli) *)
let test_time _ =
  let f () =
    Unix.sleep 1;
    42
  in
  let t, result = time f in
  assert_equal 42 result ~printer:string_of_int;
  assert_bool
    (Printf.sprintf "Expected time around 1.0, got %f" t)
    (t >= 0.9 && t <= 1.1)

let test_log_time _ =
  let f () =
    Unix.sleep 1;
    "test"
  in
  let result = log_time ~precision:2 ~msg:"Testing" f in
  assert_equal "test" result ~printer:Fun.id

let test_log_time_no_precision _ =
  let f () =
    Unix.sleep 1;
    "test"
  in
  let result = log_time ~msg:"Testing" f in
  assert_equal "test" result ~printer:Fun.id

let test_log_time_no_message _ =
  let f () =
    Unix.sleep 1;
    "test"
  in
  let result = log_time ~msg:"Testing" f in
  assert_equal "test" result ~printer:Fun.id

(** Tests for Moderation module (moderation.mli) *)
let test_check_text_length_valid _ =
  let result = check_text_length 10 "Hello" in
  assert_bool "Expected valid text length" (is_valid result)

let test_check_text_length_invalid _ =
  let result = check_text_length 5 "Too long text" in
  assert_bool "Expected invalid text length" (not (is_valid result));
  assert_equal "Text exceeds maximum length of 5 characters"
    (get_failure_reason result)

let test_contains_banned_words_clean _ =
  let result = contains_banned_words "Hello world" in
  assert_bool "Expected text without banned words" (is_valid result)

let test_moderate_text_valid _ =
  let result = moderate_text ~max_length:20 "Hello world" in
  assert_bool "Expected valid moderated text" (is_valid result)

let test_moderate_text_invalid_length _ =
  let result = moderate_text ~max_length:5 "Too long text" in
  assert_bool "Expected invalid moderated text" (not (is_valid result));
  assert_equal "Text exceeds maximum length of 5 characters"
    (get_failure_reason result)

let test_check_text_length_empty _ =
  let result = check_text_length 10 "" in
  assert_bool "Empty text should be valid" (is_valid result)

let test_check_text_length_exact _ =
  let result = check_text_length 5 "12345" in
  assert_bool "Text exactly at max length should be valid" (is_valid result)

let test_check_text_length_unicode _ =
  let result = check_text_length 10 "🌟🌙✨" in
  (* Each emoji is 4 bytes *)
  assert_bool "Unicode characters should be counted by their byte length"
    (not (is_valid result));
  assert_equal "Text exceeds maximum length of 10 characters"
    (get_failure_reason result)

let test_check_text_length_whitespace _ =
  let result = check_text_length 5 "   " in
  assert_bool "Whitespace only text should be valid if within length"
    (is_valid result)

let test_contains_banned_words_empty _ =
  let result = contains_banned_words "" in
  assert_bool "Empty text should be valid" (is_valid result)

let test_contains_banned_words_whitespace _ =
  let result = contains_banned_words "   " in
  assert_bool "Whitespace only text should be valid" (is_valid result)

let test_contains_banned_words_mixed_case _ =
  let result = contains_banned_words "BaDwOrD ReAlLyBadWoRd" in
  assert_bool "Mixed case banned words should be detected"
    (not (is_valid result));
  assert_equal "Text contains inappropriate language"
    (get_failure_reason result)

let test_contains_banned_words_substring _ =
  let result = contains_banned_words "assignment" in
  assert_bool "Substrings should not trigger banned words" (is_valid result)

let test_moderate_text_empty _ =
  let result = moderate_text ~max_length:10 "" in
  assert_bool "Empty text should be valid" (is_valid result)

let test_moderate_text_max_length_zero _ =
  let result = moderate_text ~max_length:0 "" in
  assert_bool "Empty text with zero max length should be valid"
    (is_valid result)

let test_moderate_text_max_length_negative _ =
  let result = moderate_text ~max_length:(-1) "test" in
  assert_bool "Text with negative max length should be invalid"
    (not (is_valid result));
  assert_equal "Text exceeds maximum length of -1 characters"
    (get_failure_reason result)

let test_moderate_text_multiple_banned_words _ =
  let result = moderate_text ~max_length:50 "badword reallybadword worstword" in
  assert_bool "Multiple banned words should be detected" (not (is_valid result));
  assert_equal "Text contains inappropriate language"
    (get_failure_reason result)

let test_moderate_text_just_under_limit _ =
  let text = String.make 999 'a' in
  let result = moderate_text text in
  assert_bool "Text just under default limit should be valid" (is_valid result)

let test_moderate_text_exactly_at_limit _ =
  let text = String.make 1000 'a' in
  let result = moderate_text text in
  assert_bool "Text exactly at default limit should be valid" (is_valid result)

let test_moderate_text_just_over_limit _ =
  let text = String.make 1001 'a' in
  let result = moderate_text text in
  assert_bool "Text just over default limit should be invalid"
    (not (is_valid result));
  assert_equal "Text exceeds maximum length of 1000 characters"
    (get_failure_reason result)

(** Additional test cases for get_failure_reason *)
let test_check_text_length_valid _ =
  let result = check_text_length 10 "Hello" in
  assert_bool "Expected valid text length" (is_valid result)

let test_check_text_length_invalid _ =
  let result = check_text_length 5 "Too long text" in
  assert_bool "Expected invalid text length" (not (is_valid result));
  assert_equal "Text exceeds maximum length of 5 characters"
    (get_failure_reason result)

let test_contains_banned_words_clean _ =
  let result = contains_banned_words "Hello world" in
  assert_bool "Expected text without banned words" (is_valid result)

let test_contains_banned_words_banned _ =
  let result = contains_banned_words "fuck shit badussy masturbate" in
  assert_bool "Expected text with banned words" (not (is_valid result));
  assert_equal "Text contains inappropriate language"
    (get_failure_reason result)

let test_moderate_text_valid _ =
  let result = moderate_text ~max_length:20 "Hello world" in
  assert_bool "Expected valid moderated text" (is_valid result)

let test_moderate_text_invalid_length _ =
  let result = moderate_text ~max_length:5 "Too long text" in
  assert_bool "Expected invalid moderated text" (not (is_valid result));
  assert_equal "Text exceeds maximum length of 5 characters"
    (get_failure_reason result)

let test_moderate_text_invalid_content _ =
  let result = moderate_text ~max_length:20 "badword baddestword" in
  assert_bool "Expected invalid moderated text" (not (is_valid result));
  assert_equal "Text contains inappropriate language"
    (get_failure_reason result)

(** Note: Many transformer.mli functions are not tested here because they
    require a fully initialized model and training data. We test what we can
    below. *)

(** Tests for Transformer module (transformer.mli) *)
let test_is_repetitive _ =
  assert_bool "Should detect repetition"
    (is_repetitive [| 1; 2; 1; 2; 1; 2 |] 2);
  assert_bool "Should not detect repetition"
    (not (is_repetitive [| 1; 2; 3; 4; 5; 6 |] 2))

let test_clean_text _ =
  assert_equal "hello world!" (clean_text "Hello, World!") ~printer:Fun.id;
  assert_equal "testing  special  chars!"
    (clean_text "Testing @#$% Special &*() Chars!")
    ~printer:Fun.id

let test_sample_with_temperature _ =
  let distribution = [| 0.1; 0.8; 0.1 |] in
  let samples =
    List.init 100 (fun _ -> sample_with_temperature distribution 0.1)
  in
  (* With low temperature, should mostly pick index 1 which has highest
     probability *)
  assert_bool "Should mostly pick highest probability"
    (List.length (List.filter (fun x -> x = 1) samples) > 50)

(** Tests for Tokenizer module (tokenizer.mli) *)
let test_tokenizer_encode_decode _ =
  let original_text = "hello world" in
  let tokens = encode original_text in
  let decoded_text = decode tokens in
  assert_equal original_text decoded_text ~printer:Fun.id

let test_tokenizer_multiple_words _ =
  let original_text = "the quick brown fox jumps over the lazy dog" in
  let tokens = encode original_text in
  let decoded_text = decode tokens in
  assert_equal original_text decoded_text ~printer:Fun.id

let test_tokenizer_repeated_words _ =
  let original_text = "hello hello world world" in
  let tokens = encode original_text in
  let decoded_text = decode tokens in
  assert_equal original_text decoded_text ~printer:Fun.id

let test_tokenizer_unknown_token _ =
  let tokens = [| 999999 |] in
  (* Using an ID that shouldn't exist in vocab *)
  let decoded_text = decode tokens in
  assert_equal "<UNK>" decoded_text ~printer:Fun.id

(** Tests for Model module (model.mli) *)
let test_model_not_initialized _ =
  match generate_sample () with
  | Error _ -> () (* Expected: error when not initialized *)
  | Ok _ -> assert_failure "Expected error when model not initialized"

let test_model_generate_text_not_initialized _ =
  match generate_text 10 with
  | Error _ -> () (* Expected: error when not initialized *)
  | Ok _ -> assert_failure "Expected error when model not initialized"

let test_model_generate_text_invalid_length _ =
  match generate_text (-5) with
  | Error _ -> () (* Expected: error for negative length *)
  | Ok _ -> assert_failure "Expected error for negative length"

let test_model_generate_text_with_seed _ =
  match generate_text ~seed:"test seed" 10 with
  | Error _ -> () (* Expected: error when not initialized *)
  | Ok _ -> assert_failure "Expected error when model not initialized"

(** Tests for Pretrain module (pretrain.mli) *)
let test_load_dataset _ =
  (* Create a temporary file with test data *)
  let temp_file = Filename.temp_file "test" ".txt" in
  let oc = open_out temp_file in
  output_string oc "hello world\ntesting data\nexample text\n";
  close_out oc;

  let dataset = load_dataset temp_file in
  assert_equal [ "hello world"; "testing data"; "example text" ] dataset;
  Sys.remove temp_file

let test_create_batches _ =
  let dataset = [ "hello world"; "testing data"; "example text" ] in
  let batches = create_batches dataset 2 in
  (* The function creates 2 batches regardless of input size *)
  assert_equal 2 (List.length batches) ~printer:string_of_int;
  let first_batch = List.nth batches 0 in
  let second_batch = List.nth batches 1 in
  (* Check that we have 2 items total *)
  let total_items = Array.length first_batch + Array.length second_batch in
  assert_equal 2 total_items ~printer:string_of_int;
  (* Each batch should have 1 item *)
  assert_equal 1 (Array.length first_batch) ~printer:string_of_int;
  assert_equal 1 (Array.length second_batch) ~printer:string_of_int

let test_cross_entropy_loss _ =
  let output = of_array [| [| 0.7; 0.2; 0.1 |] |] in
  let target = 0 in
  (* First class *)
  let loss = cross_entropy_loss output target in
  assert_bool
    (Printf.sprintf "Expected loss around 0.77, got %f" loss)
    (abs_float (loss -. 0.77) < 0.01)

let () =
  run_test_tt_main
    ("test suite"
    >::: [
           (* Matrix module tests *)
           "test_matrix_dot" >:: test_matrix_dot;
           "test_matrix_dot_single" >:: test_matrix_dot_single;
           "test_matrix_dot_fail" >:: test_matrix_dot_fail;
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
           "test_matrix_mean" >:: test_matrix_mean;
           "test_matrix_var_1" >:: test_matrix_var_1;
           "test_matrix_var_2" >:: test_matrix_var_2;
           "test_matrix_vec_sum" >:: test_matrix_vec_sum;
           "test_matrix_add_vec" >:: test_matrix_add_vec;
           "test_matrix_divide_in_place" >:: test_matrix_divide_in_place;
           "test_dot_transpose_and_scale" >:: test_dot_transpose_and_scale;
           "test_mat_dot_vec" >:: test_mat_dot_vec;
           "test_relu_in_place" >:: test_relu_in_place;
           "test_add" >:: test_add;
           "test_scale" >:: test_scale;
           "test_length" >:: test_length;
           "test_lacaml_matrix_conversion" >:: test_lacaml_matrix_conversion;
           "test_lacaml_vector_conversion" >:: test_lacaml_vector_conversion;
           "test_softmax_empty" >:: test_softmax_empty;
           "test_softmax_single_value" >:: test_softmax_single_value;
           "test_relu_in_place_empty" >:: test_relu_in_place_empty;
           "test_relu_in_place_all_negative" >:: test_relu_in_place_all_negative;
           "test_concat_empty_matrices" >:: test_concat_empty_matrices;
           "test_concat_single_matrix" >:: test_concat_single_matrix;
           "test_add_with_zero_matrix" >:: test_add_with_zero_matrix;
           "test_elementwise_mul_with_zero_matrix"
           >:: test_elementwise_mul_with_zero_matrix;
           (* Util module tests *)
           "test_time" >:: test_time;
           "test_log_time" >:: test_log_time;
           "test_log_time_no_message" >:: test_log_time_no_message;
           "test_log_time_no_precision" >:: test_log_time_no_precision;
           (* Moderation module tests *)
           "test_check_text_length_valid" >:: test_check_text_length_valid;
           "test_check_text_length_invalid" >:: test_check_text_length_invalid;
           "test_contains_banned_words_clean"
           >:: test_contains_banned_words_clean;
           "test_contains_banned_words_banned"
           >:: test_contains_banned_words_banned;
           "test_moderate_text_valid" >:: test_moderate_text_valid;
           "test_moderate_text_invalid_length"
           >:: test_moderate_text_invalid_length;
           "test_moderate_text_invalid_content"
           >:: test_moderate_text_invalid_content;
           "test_check_text_length_empty" >:: test_check_text_length_empty;
           "test_check_text_length_exact" >:: test_check_text_length_exact;
           "test_check_text_length_unicode" >:: test_check_text_length_unicode;
           "test_check_text_length_whitespace"
           >:: test_check_text_length_whitespace;
           "test_contains_banned_words_empty"
           >:: test_contains_banned_words_empty;
           "test_contains_banned_words_whitespace"
           >:: test_contains_banned_words_whitespace;
           "test_contains_banned_words_mixed_case"
           >:: test_contains_banned_words_mixed_case;
           "test_contains_banned_words_substring"
           >:: test_contains_banned_words_substring;
           "test_moderate_text_empty" >:: test_moderate_text_empty;
           "test_moderate_text_max_length_zero"
           >:: test_moderate_text_max_length_zero;
           "test_moderate_text_max_length_negative"
           >:: test_moderate_text_max_length_negative;
           "test_moderate_text_multiple_banned_words"
           >:: test_moderate_text_multiple_banned_words;
           "test_moderate_text_just_under_limit"
           >:: test_moderate_text_just_under_limit;
           "test_moderate_text_exactly_at_limit"
           >:: test_moderate_text_exactly_at_limit;
           "test_moderate_text_just_over_limit"
           >:: test_moderate_text_just_over_limit;
           (* Transformer module tests *)
           "test_is_repetitive" >:: test_is_repetitive;
           "test_clean_text" >:: test_clean_text;
           "test_sample_with_temperature" >:: test_sample_with_temperature;
           (* Tokenizer module tests *)
           "test_tokenizer_encode_decode" >:: test_tokenizer_encode_decode;
           "test_tokenizer_multiple_words" >:: test_tokenizer_multiple_words;
           "test_tokenizer_repeated_words" >:: test_tokenizer_repeated_words;
           "test_tokenizer_unknown_token" >:: test_tokenizer_unknown_token;
           (* Model module tests *)
           "test_model_not_initialized" >:: test_model_not_initialized;
           "test_model_generate_text_not_initialized"
           >:: test_model_generate_text_not_initialized;
           "test_model_generate_text_invalid_length"
           >:: test_model_generate_text_invalid_length;
           "test_model_generate_text_with_seed"
           >:: test_model_generate_text_with_seed;
           (* Pretrain module tests *)
           "test_load_dataset" >:: test_load_dataset;
           "test_create_batches" >:: test_create_batches;
           "test_cross_entropy_loss" >:: test_cross_entropy_loss;
         ])
