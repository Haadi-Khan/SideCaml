type t

val apply : t -> t -> t -> t

val of_array : float array array -> t
(** [of_array a] creates a layer from a 2D array [a]. *)

val to_array : t -> float array array
(** [to_array layer] converts the layer [layer] to a 2D array. *)

val of_matrix : Matrix.t -> t
(** [of_matrix matrix] creates a layer from a matrix [matrix]. *)
