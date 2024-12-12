type mat
(** Type alias for a 2D matrix of floats. *)

type vec
(** Type alias for a vector of floats. *)

val dot : mat -> mat -> mat
(** [dot a b] computes the inner product of matrices [a] and [b]. It is up to
    the caller to make sure that [snd (size a) = fst (size b)]. *)

val dot_transpose_and_scale : mat -> mat -> float -> mat
(** [dot_transpose a b t] computes the inner product of [a] and the transpose of
    [b], then scaled by [t]. Make sure that [snd (size a) = snd (size b)]. *)

val mat_dot_vec : mat -> vec -> vec
(** [dot m v] computes the product of the matrix [m] with the vector [v]. Make
    sure that [snd (size m) = length v]. *)

val mean : mat -> vec
(** [mean m] computes the mean of each row of [m]. *)

val var : ?mean_:vec -> mat -> vec
(** [var m] computes the variance of each row of [m]. The optional parameter
    [mean_] can be passed in if the mean is already computed, to speed up the
    calculation.*)

val softmax : mat -> mat
(** [softmax a] computes the softmax of each row in the matrix [a]. *)

val relu_in_place : mat -> unit
(** [relu a] computes the ReLU (max(0, x)) of each element in the matrix [a] in
    place. *)

val reshape : mat -> int -> int -> mat
(** [reshape a rows cols] reshapes the matrix [a] into a new matrix with the
    given dimensions ([rows] x [cols]). Raises [Invalid_argument] if the total
    number of elements does not match. *)

val concat : mat array -> mat
(** [concat matrices] concatenates an array of matrices along their columns.
    Assumes all matrices have the same number of rows. *)

val map : (float -> float) -> mat -> mat
(** [map f matrix] applies the unary function [f] element-wise to the matrix
    [matrix]. *)

val to_array : mat -> float array array
(** [to_array a] converts the matrix [a] to a 2D array. *)

val of_array : float array array -> mat
(** [of_array a] creates a matrix from a 2D array [a]. *)

val vec_to_array : vec -> float array
(** [vec_to_array v] converts the vector [v] to an array. *)

val vec_of_array : float array -> vec
(** [vec_of_array a] converts the array [a] to a vector. *)

val one_hot : int -> int -> mat
(** [one_hot index size] creates a one-hot vector of size [size] with a 1 at the
    given [index]. *)

val get_row : mat -> int -> vec
(** [get_row matrix i] returns the [i]-th row of the matrix [matrix] as a
    vector. *)

val sum : mat -> float
(** [sum a] computes the sum of all elements in the matrix [a]. *)

val vec_sum : vec -> float
(** [vec_sum v] computes the sum of all elements in the vector [v]. *)

val mat_add_vec : mat -> float -> vec -> mat
(** [mat_add_vec m alpha v] adds [alpha v] to each column of [m].*)

val add : mat -> float -> mat -> mat
(** [add m1 alpha m2] returns the sum of [m1] and [alpha m2].*)

val divide_in_place : mat -> float -> unit
(** [divide_in_place m t] divides every element of [m] by [t], modifying [m] in
    place. *)

val elementwise_mul : mat -> mat -> mat
(** [elementwise_mult a b] computes the element-wise product of matrices [a] and
    [b]. Raises [Invalid_argument] if the dimensions are incompatible. *)

val ones : int * int -> mat
(** [ones (rows, cols)] creates a matrix of size [rows] by [cols] filled with
    ones. *)

val random : int -> int -> mat
(** [random rows cols] creates a matrix of size [rows] by [cols] filled with
    random values between -1 and 1. *)

val get : mat -> int -> int -> float

val size : mat -> int * int
(** [size m] returns the [(rows, cols)] of the matrix [m].*)

val length : vec -> int
(** [length v] is the length of the vector [v].*)

val to_lacaml_matrix : mat -> Lacaml.D.mat
(** [to_lacaml_matrix m] converts [m] to a Lacaml matrix. *)

val of_lacaml_matrix : Lacaml.D.mat -> mat
(** [of_lacaml_matrix m] converts from a Lacaml matrix to a [mat]. *)

val to_lacaml_vector : vec -> Lacaml.D.vec
(** [to_lacaml_vector v] converts [v] to a Lacaml vector. *)

val of_lacaml_vector : Lacaml.D.vec -> vec
(** [of_lacaml_vector m] converts from a Lacaml vector to a [vec]. *)
