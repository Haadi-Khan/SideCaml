type t
(** Type alias for a 2D matrix of floats. *)

val dot : t -> t -> t
(** [dot a b] computes the dot product of matrices [a] and [b]. Raises
    [Invalid_argument] if the dimensions are incompatible. *)

val transpose : t -> t
(** [transpose a] computes the transpose of the matrix [a]. *)

val scale : t -> float -> t
(** [scale a s] scales all elements of the matrix [a] by the scalar [s]. *)

val apply2 : (float -> float -> float) -> t -> t -> t
(** [apply2 f a b] applies the binary function [f] element-wise to matrices [a]
    and [b]. Raises [Invalid_argument] if the dimensions of [a] and [b] are not
    the same. *)

val softmax : t -> t
(** [softmax a] computes the softmax of each row in the matrix [a]. *)

val reshape : t -> int array -> t
(** [reshape a dims] reshapes the matrix [a] into a new matrix with the given
    dimensions [dims]. Raises [Invalid_argument] if the total number of elements
    does not match. *)

val concat : t list -> t
(** [concat matrices] concatenates a list of matrices along their columns.
    Assumes all matrices have the same number of rows. *)

val map : (float -> float) -> t -> t
(** [map f matrix] applies the unary function [f] element-wise to the matrix
    [matrix]. *)

val to_array : t -> float array array
(** [to_array a] converts the matrix [a] to a 2D array. *)

val of_array : float array array -> t
(** [of_array a] creates a matrix from a 2D array [a]. *)
<<<<<<< HEAD

val one_hot : int -> int -> t
(** [one_hot index size] creates a one-hot vector of size [size] with a 1 at the
    given [index]. *)

val get_row : t -> int -> float array
(** [get_row matrix i] returns the [i]-th row of the matrix [matrix]. *)

val sum : t -> float
(** [sum a] computes the sum of all elements in the matrix [a]. *)

val map2 : (float -> float -> float) -> t -> t -> t
(** [map2 f a b] applies the binary function [f] element-wise to matrices [a]
    and [b]. Raises [Invalid_argument] if the dimensions of [a] and [b] are not
    the same. *)

val elementwise_mul : t -> t -> t
(** [elementwise_mult a b] computes the element-wise product of matrices [a] and
    [b]. Raises [Invalid_argument] if the dimensions are incompatible. *)

val ones : int * int -> t
(** [ones (rows, cols)] creates a matrix of size [rows] by [cols] filled with
    ones. *)

val random : int -> int -> t
(** [random rows cols] creates a matrix of size [rows] by [cols] filled with
    random values between -1 and 1. *)

val get : t -> int -> int -> float
=======
>>>>>>> refs/remotes/origin/master
