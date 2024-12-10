type t
type post

val load_posts : string -> post list
(** [load_posts filename] loads a list of posts from a JSON file. *)

val prepare_training_data : post list -> string
(** [prepare_training_data posts] extracts the text from a list of posts and concatenates them into a single string. *)

val scaled_dot_product_attention :
  float array array ->
  float array array ->
  Matrix.t ->
  Matrix.t option ->
  Matrix.t
(** [scaled_dot_product_attention query key value mask] computes the scaled dot product attention. *)

val multi_head_attention :
  t ->
  Matrix.t ->
  Matrix.t ->
  Matrix.t ->
  Matrix.t option ->
  Matrix.t
(** [multi_head_attention config query key value mask] computes the multi-head attention. *)

val transformer_block : t -> Matrix.t -> Matrix.t
(** [transformer_block config input] applies a transformer block to the input matrix. *)

val prepare_input : 'a array -> Matrix.t
(** [prepare_input input] converts an array of arrays into a matrix. *)

val sample_from_distribution : float array -> int
(** [sample_from_distribution distribution] samples an index from a categorical distribution. *)

val sample_with_temperature : float array -> float -> int
(** [sample_with_temperature distribution temperature] samples an index from a categorical distribution with a temperature. *)

val beam_search : float array -> int -> int -> int
(** [beam_search distribution beam_size max_length] performs beam search to generate a sequence of tokens. *)

val is_repetitive : int array -> int -> bool
(** [is_repetitive sequence n] checks if the last n tokens in the sequence are repetitive. *)

val forward_pass : t -> int array -> float array
(** [forward_pass model input] computes the forward pass of the model. *)

val generate_text : t -> unit -> string -> int -> string
(** [generate_text model seed length] generates text using the model. *)

val init_transformer : unit -> t
(** [init_transformer ()] initializes a transformer model. *)

val get_random_first_word : string -> string
(** [get_random_first_word text] gets a random word from the text. *)

val generate_sample : unit -> string
(** [generate_sample ()] generates a sample post. *)

val clean_text : string -> string
(** [clean_text text] cleans the text by removing special characters. *)

val position_encoding : int -> int -> Matrix.t
(** [position_encoding max_length embedding_dim] computes the position encoding matrix. *)
