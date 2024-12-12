open Core
open Yojson.Basic.Util
open Matrix
open Tokenizer
open Transformer

type t = {
  batch_size : int;
  learning_rate : float;
  max_epochs : int;
  checkpoint_dir : string;
}

let load_model filename =
  let ic = In_channel.create filename in
  let model = Marshal.from_channel ic in
  In_channel.close ic;
  model

(* Save model parameters to a file *)
let save_model config filename =
  let oc = Out_channel.create filename in
  Marshal.to_channel oc config [];
  Out_channel.close oc

(* Load and preprocess dataset *)
let load_dataset filename =
  let ic = In_channel.create filename in
  let lines = ref [] in
  try
    while true do
      lines := In_channel.input_line_exn ic :: !lines
    done;
    !lines
  with End_of_file ->
    In_channel.close ic;
    List.rev !lines

(* Create batches *)
let create_batches texts batch_size =
  Printf.printf "Creating batches from %d texts with batch size %d\n%!"
    (List.length texts) batch_size;

  let tokens = Array.of_list (List.map ~f:encode texts) in
  Printf.printf "Encoded tokens array length: %d\n%!" (Array.length tokens);

  let batches =
    Array.init batch_size ~f:(fun i ->
        Printf.printf "Creating batch %d/%d\n%!" (i + 1) batch_size;
        Array.init
          (Array.length tokens / batch_size)
          ~f:(fun j -> tokens.(i + (j * batch_size))))
  in

  Printf.printf "Created %d batches of size %d\n%!" (Array.length batches)
    (if Array.length batches > 0 then Array.length batches.(0) else 0);

  Array.to_list batches

(* Calculate loss *)
let cross_entropy_loss predicted target =
  let log_softmax = Matrix.map Float.log (Matrix.softmax predicted) in
  let _, predicted_cols = Matrix.size predicted in
  let target_dist = Matrix.one_hot target predicted_cols in
  -.Matrix.sum (Matrix.elementwise_mul log_softmax target_dist)

(* Calculate gradients with respect to loss *)
let calculate_gradients loss =
  (* Simple implementation - assuming scalar loss *)
  Matrix.ones (1, 1)
(* Replace with actual gradient computation *)

(* Update parameters using gradient descent *)
let update_parameters params gradients =
  Transformer.update_weights params gradients

(* Training loop *)
let train config t dataset =
  Printf.printf "Starting training with %d epochs\n" t.max_epochs;
  Printf.printf "Batch size: %d\n" t.batch_size;

  let batches = create_batches dataset t.batch_size in
  Printf.printf "Created %d batches\n" (List.length batches);

  let model_params = ref config in

  for epoch = 1 to t.max_epochs do
    Printf.printf "\nEpoch %d:\n" epoch;
    let total_loss = ref 0. in

    List.iteri batches ~f:(fun batch_idx batch ->
        Printf.printf "\nProcessing batch %d/%d\n" (batch_idx + 1)
          (List.length batches);

        (* Forward pass *)
        Printf.printf "Forward pass - Input batch size: %d\n"
          (Array.length batch);
        let logits =
          let batch_size = Array.length batch in
          List.mapi
            ~f:(fun i input ->
              Printf.printf "Forward pass %d/%d for input of size %d %!" (i + 1)
                batch_size (Array.length input);
              Util.log_time (fun () -> forward_pass !model_params input))
            (Array.to_list batch)
        in
        Printf.printf "Forward pass complete - Output logits size: %d\n"
          (List.length logits);

        (* Print sample logits *)
        (match List.hd logits with
        | Some first_logit ->
            Printf.printf "Sample logit shape: %d\n" (Matrix.length first_logit);
        | None ->
            Printf.printf "No logits produced!\n");

        (* Calculate loss *)
        let batch_loss =
          List.fold ~init:0.
            ~f:(fun acc (pred, target) ->
              let loss =
                cross_entropy_loss
                  (Matrix.of_array [| Matrix.vec_to_array pred |])
                  (Array.get target 0)
              in
              (* Printf.printf "Sample loss: %f\n%!" loss; *)
              acc +. loss)
            (List.zip_exn logits (Array.to_list batch))
        in
        Printf.printf "Batch %d loss: %f\n" (batch_idx + 1) batch_loss;

        (* Backward pass & update *)
        let gradients = calculate_gradients batch_loss in
        Printf.printf "Calculated gradients\n";

        model_params :=
          update_parameters !model_params t.learning_rate gradients;
        Printf.printf "Updated model parameters\n");

    Printf.printf "\nEpoch %d complete - Average loss: %f\n" epoch
      (!total_loss /. float_of_int (List.length batches));
  done

let training_config batch_size learning_rate max_epochs checkpoint_dir =
  { batch_size; learning_rate; max_epochs; checkpoint_dir }
