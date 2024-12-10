type t
type post

val load_posts : string -> post list
val prepare_training_data : post list -> string
val scaled_dot_product_attention :
  float array array ->
  float array array ->
  Matrix.t ->
  Matrix.t option ->
  Matrix.t
val multi_head_attention :
  t ->
  Matrix.t ->
  Matrix.t ->
  Matrix.t ->
  Matrix.t option ->
  Matrix.t
val transformer_block : t -> Matrix.t -> Matrix.t
val prepare_input : 'a array -> Matrix.t
val sample_from_distribution : float array -> int
val sample_with_temperature : float array -> float -> int
val beam_search : float array -> int -> int -> int
val is_repetitive : int array -> int -> bool
val forward_pass : t -> int array -> float array
val generate_text : t -> unit -> string -> int -> string
val init_transformer : unit -> t
val get_random_first_word : string -> string
val generate_sample : unit -> string
val clean_text : string -> string
val position_encoding : int -> int -> Matrix.t