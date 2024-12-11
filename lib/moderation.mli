type t
(** Moderation module for filtering and checking text content *)

val check_text_length : int -> string -> t
(** [check_text_length max_length text] checks if text length is within limits *)

val contains_banned_words : string -> t
(** [contains_banned_words text] checks if text contains any banned words *)

val moderate_text : ?max_length:int -> string -> t
(** [moderate_text ?max_length text] performs all moderation checks on the text *)

val get_failure_reason : t -> string
(** [get_failure_reason result] returns the reason for moderation failure, or "Unknown reason" if none provided *)

val is_valid : t -> bool
(** [is_valid result] returns true if the moderation result is valid *)
