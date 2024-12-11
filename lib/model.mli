type t

val generate_sample : unit -> (string, t) result
(** [generate_sample ()] generates a moderated sample of text using default
    parameters *)

val generate_text : ?max_length:int -> ?seed:string -> int -> (string, t) result
(** [generate_text ?max_length ?seed length] generates text of specified length
    with optional max length restriction and seed text. Returns moderated text
    that passes all checks. *)

val init : unit -> (unit, t) result
(** [init ()] initializes the model and required resources *)
