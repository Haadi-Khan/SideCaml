type t

val apply : t -> t
(** [apply layer] applies normalization to the layer. *)

val of_matrix : Matrix.mat -> t
(** [of_matrix matrix] creates a layer from a matrix [matrix]. *)

val to_matrix : t -> Matrix.mat
(** [to_matrix layer] converts the layer [layer] to a matrix. *)
