val time : (unit -> 'a) -> float * 'a
(** [time f] returns [(t, f ())], where [t] is how long [f] took to complete in
    seconds.*)

val log_time :
  ?precision:int -> ?msg:string -> ?indent:int -> (unit -> 'a) -> 'a
(** [log_time msg indent f ~precision] returns [f ()] and prints out a message
    offset by [indent] spaces, starting with [msg] and also stating how long [f]
    took to complete in seconds, truncated to [precision] decimal places. *)
