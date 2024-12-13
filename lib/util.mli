val time : (unit -> 'a) -> float * 'a
(** [time f] returns [(t, f ())], where [t] is how long [f] took to complete in
    seconds.*)

val log_time : ?precision:int -> ?msg:string -> (unit -> 'a) -> 'a
(** [log_time msg f ~precision] returns [f ()] and prints out a message starting
    with [msg] that also states how long [f] took to complete in seconds,
    truncated to [precision] decimal places. *)
