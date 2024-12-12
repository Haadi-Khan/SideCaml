open Transformer
open Pretrain

type t = {
  batch_size : int;
  learning_rate : float;
  max_epochs : int;
  checkpoint_dir : string;
}

type post = Transformer.post

(* Fine-tuning function *)
let fine_tune config training_config =
  let posts = Transformer.load_posts "data/posts.json" in
  let texts = List.map Transformer.get_text posts in
  Pretrain.train config training_config texts
