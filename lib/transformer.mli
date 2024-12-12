type t
type post

val load_posts : string -> post list
(** [load_posts filename] loads a list of posts from a JSON file. *)

val prepare_training_data : post list -> string
(** [prepare_training_data posts] extracts the text from a list of posts and
    concatenates them into a single string. *)

val scaled_dot_product_attention :
  Matrix.mat -> Matrix.mat -> Matrix.mat -> Matrix.mat option -> Matrix.mat
(** [scaled_dot_product_attention query key value mask] computes the scaled dot
    product attention. *)

val multi_head_attention :
  t -> Matrix.mat -> Matrix.mat -> Matrix.mat -> Matrix.mat option -> Matrix.mat
(** [multi_head_attention config query key value mask] computes the multi-head
    attention. *)

val transformer_block : t -> Matrix.mat -> Matrix.mat
(** [transformer_block config input] applies a transformer block to the input
    matrix. *)

val sample_from_distribution : float array -> int
(** [sample_from_distribution distribution] samples an index from a categorical
    distribution. *)

val sample_with_temperature : float array -> float -> int
(** [sample_with_temperature distribution temperature] samples an index from a
    categorical distribution with a temperature. *)

val beam_search : float array -> int -> int -> int
(** [beam_search distribution beam_size max_length] performs beam search to
    generate a sequence of tokens. *)

val is_repetitive : int array -> int -> bool
(** [is_repetitive sequence n] checks if the last n tokens in the sequence are
    repetitive. *)

val forward_pass : t -> int array -> Matrix.vec
(** [forward_pass model input] computes the forward pass of the model. *)

val generate_text : t -> unit -> string -> int -> string
(** [generate_text model seed length] generates text using the model. *)

val init_transformer : unit -> t
(** [init_transformer ()] initializes a transformer model. *)

val get_random_first_word : string -> string
(** [get_random_first_word text] gets a random word from the text. *)

val clean_text : string -> string
(** [clean_text text] cleans the text by removing special characters. *)

val position_encoding : int -> int -> Matrix.mat
(** [position_encoding max_length embedding_dim] computes the position encoding
    matrix. *)

val update_weights : t -> float -> Matrix.mat -> t
(** [update_weights params learning_rate gradients] updates the model parameters
    using gradient descent. *)

val get_text : post -> string
(** [get_text post] gets the text from a post. *)
