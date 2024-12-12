let time f =
  let start_time = Core.Time_ns.now () in
  let res = f () in
  let elapsed = Core.Time_ns.(Span.to_sec (diff (now ()) start_time)) in
  (elapsed, res)
[@@inline]

let log_time ?(precision = 3) ?(msg = "") f =
  let elapsed, res = time f in
  Printf.printf "%s[done in %.*fs]\n%!" msg precision
    elapsed;
  res
[@@inline]
