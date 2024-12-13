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
[@@coverage off]

(* Save model parameters to a file *)
let save_model config filename =
  let oc = Out_channel.create filename in
  Marshal.to_channel oc config [];
  Out_channel.close oc
[@@coverage off]

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
  let open Matrix in
  let log_softmax = map Float.log (softmax predicted) in
  let _, predicted_cols = size predicted in
  let target_dist = one_hot target predicted_cols in
  -.sum (elementwise_mul log_softmax target_dist)

(* Calculate gradients with respect to loss *)
let calculate_gradients loss = Matrix.ones (1, 1) [@@coverage off]

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
        (* Printf.printf "Forward pass - Input batch size: %d\n" *)
        (* Array.length batch; *)
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
            Printf.printf "Sample logit shape: %d\n" (Matrix.length first_logit)
        | None -> Printf.printf "No logits produced!\n");

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
        total_loss := !total_loss +. batch_loss;

        (* Backward pass & update *)
        let gradients = calculate_gradients batch_loss in
        Printf.printf "Calculated gradients\n";

        model_params :=
          Transformer.update_weights !model_params t.learning_rate gradients);

    (* Add checkpoint saving at end of each epoch *)
    begin
      let checkpoint_file =
        Printf.sprintf "%s/model_epoch_%d.ckpt" t.checkpoint_dir epoch
      in
      Printf.printf "Saving checkpoint to %s\n" checkpoint_file;
      save_model !model_params checkpoint_file
    end;

    Printf.printf "\nEpoch %d complete - Average loss: %f\n" epoch
      (!total_loss /. float_of_int (List.length batches))
  done;

  (* Save final model *)
  let final_model_path =
    Printf.sprintf "%s/model_final.ckpt" t.checkpoint_dir
  in
  Printf.printf "Saving final model to %s\n" final_model_path;
  save_model !model_params final_model_path
[@@coverage off]

let training_config batch_size learning_rate max_epochs checkpoint_dir =
  { batch_size; learning_rate; max_epochs; checkpoint_dir }
[@@coverage off]
