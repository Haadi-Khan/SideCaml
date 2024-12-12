type t
(** Abstraction type for training config. *)

val load_model : string -> 'a
(** [load_model path] loads a model from a file. *)

val save_model : 'a -> string -> unit
(** [save_model model path] saves a model to a file. *)

val load_dataset : string -> string list
(** [load_dataset path] loads a dataset from a file. *)

val create_batches : string list -> int -> int array array list
(** [create_batches dataset batch_size] creates batches from a dataset. *)

val cross_entropy_loss : Matrix.mat -> int -> float
(** [cross_entropy_loss output target] computes the cross-entropy loss. *)

val calculate_gradients : 'a -> Matrix.mat
(** [calculate_gradients model input] computes the gradients of the model. *)

val train : Transformer.t -> t -> string list -> unit
(** [train model dataset] trains the model on a dataset. *)

val training_config : int -> float -> int -> string -> t
(** [training_config batch_size learning_rate max_epochs checkpoint_dir] creates
    a training config. *)
