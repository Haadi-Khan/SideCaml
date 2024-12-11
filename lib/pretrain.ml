open Core
open Yojson.Basic.Util
open Matrix
open Layernorm
open Feedforward
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
  Printf.printf "Creating batches from %d texts with batch size %d\n"
    (List.length texts) batch_size;
  Out_channel.flush stdout;

  let tokens = Array.of_list (List.map ~f:encode texts) in
  Printf.printf "Encoded tokens array length: %d\n" (Array.length tokens);
  Out_channel.flush stdout;

  let batches =
    Array.init batch_size ~f:(fun i ->
        Printf.printf "Creating batch %d/%d\n" (i + 1) batch_size;
        Out_channel.flush stdout;
        Array.init
          (Array.length tokens / batch_size)
          ~f:(fun j -> tokens.(i + (j * batch_size))))
  in

  Printf.printf "Created %d batches of size %d\n" (Array.length batches)
    (if Array.length batches > 0 then Array.length batches.(0) else 0);
  Out_channel.flush stdout;

  Array.to_list batches

(* Calculate loss *)
let cross_entropy_loss predicted target =
  let log_softmax = Matrix.map Float.log (Matrix.softmax predicted) in
  let target_dist =
    Matrix.one_hot target (Array.length (Matrix.get_row predicted 0))
  in
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
  Out_channel.flush stdout;
  Printf.printf "Batch size: %d\n" t.batch_size;
  Out_channel.flush stdout;

  let batches = create_batches dataset t.batch_size in
  Printf.printf "Created %d batches\n" (List.length batches);
  Out_channel.flush stdout;

  let model_params = ref config in

  for epoch = 1 to t.max_epochs do
    Printf.printf "\nEpoch %d:\n" epoch;
    Out_channel.flush stdout;
    let total_loss = ref 0. in

    List.iteri batches ~f:(fun batch_idx batch ->
        Printf.printf "\nProcessing batch %d/%d\n" (batch_idx + 1)
          (List.length batches);
        Out_channel.flush stdout;

        (* Forward pass *)
        Printf.printf "Forward pass - Input batch size: %d\n"
          (Array.length batch);
        Out_channel.flush stdout;
        let logits =
          let batch_size = Array.length batch in
          List.mapi
            ~f:(fun i input ->
              Printf.printf "Forward pass %d/%d for input of size %d%!" (i + 1)
                batch_size (Array.length input);
              let start_time = Time_ns.now () in
              let res = forward_pass !model_params input in
              let elapsed = Time_ns.(Span.to_sec (diff (now ()) start_time)) in
              Printf.printf " [done in %.3fs]\n%!" elapsed;
              res)
            (Array.to_list batch)
        in
        Printf.printf "Forward pass complete - Output logits size: %d\n"
          (List.length logits);
        Out_channel.flush stdout;

        (* Print sample logits *)
        (match List.hd logits with
        | Some first_logit ->
            Printf.printf "Sample logit shape: %d\n" (Array.length first_logit);
            Out_channel.flush stdout
        | None ->
            Printf.printf "No logits produced!\n";
            Out_channel.flush stdout);

        (* Calculate loss *)
        let batch_loss =
          List.fold ~init:0.
            ~f:(fun acc (pred, target) ->
              let loss =
                cross_entropy_loss
                  (Matrix.of_array [| pred |])
                  (Array.get target 0)
              in
              Printf.printf "Sample loss: %f\n" loss;
              Out_channel.flush stdout;
              acc +. loss)
            (List.zip_exn logits (List.tl_exn (Array.to_list batch)))
        in
        Printf.printf "Batch %d loss: %f\n" (batch_idx + 1) batch_loss;
        Out_channel.flush stdout;

        (* Backward pass & update *)
        let gradients = calculate_gradients batch_loss in
        Printf.printf "Calculated gradients\n";
        Out_channel.flush stdout;

        model_params :=
          update_parameters !model_params t.learning_rate gradients;
        Printf.printf "Updated model parameters\n";
        Out_channel.flush stdout);

    Printf.printf "\nEpoch %d complete - Average loss: %f\n" epoch
      (!total_loss /. float_of_int (List.length batches));
    Out_channel.flush stdout
  done

let training_config batch_size learning_rate max_epochs checkpoint_dir =
  { batch_size; learning_rate; max_epochs; checkpoint_dir }
