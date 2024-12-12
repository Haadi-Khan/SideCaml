let time f =
  let start_time = Core.Time_ns.now () in
  let res = f () in
  let elapsed = Core.Time_ns.(Span.to_sec (diff (now ()) start_time)) in
  (elapsed, res)
[@@inline]

let log_time ?(precision = 3) ?(msg = "") ?(indent = 0) f =
  let elapsed, res = time f in
  Printf.printf "%s%s[done in %.*fs]\n%!" (String.make indent ' ') msg precision
    elapsed;
  res
[@@inline]
