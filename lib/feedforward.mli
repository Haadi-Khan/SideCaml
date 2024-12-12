type t

val apply : t -> t -> t -> t

val of_matrix : Matrix.mat -> t
(** [of_matrix matrix] creates a layer from a matrix [matrix]. *)

val to_matrix : t -> Matrix.mat
(** [to_matrix layer] returns a layer [layer] as a matrix. *)
