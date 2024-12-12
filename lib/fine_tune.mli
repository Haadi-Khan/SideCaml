open Transformer
open Pretrain

type t = {
  batch_size : int;
  learning_rate : float;
  max_epochs : int;
  checkpoint_dir : string;
}

type post = Transformer.post

val fine_tune : Transformer.t -> Pretrain.t -> unit
