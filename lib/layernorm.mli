type t

val apply : t -> t
(** [apply layer] applies normalization to the layer. *)

val of_matrix : Matrix.t -> t
(** [of_matrix matrix] creates a layer from a matrix [matrix]. *)

val to_matrix : t -> Matrix.t
(** [to_matrix layer] converts the layer [layer] to a matrix. *)