type t
(** Abstract type for fine tune config*)
type post
(** Abstract type for post*)

val fine_tune : Transformer.t -> Pretrain.t -> unit
(**[fine_tune config training_config] trains a Transformer model on a set of
   posts. [config] is the configuration for the Transformer model, and
   [training_config] is the configuration for training. *)
