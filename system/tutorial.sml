structure C = SMLofNJ.Cont;
(* fun f x = C.reset (fn p => *)
(*   C.shift (p, fn k => fn () => k "hello") ^ " world") x; *)

fun either p a b = C.shift (p, fn k => (k a; k b))

val test = C.reset (fn p =>
  let val x = either p "a" "b"
  in  print ("x=" ^ x ^ "\n")
  end)
