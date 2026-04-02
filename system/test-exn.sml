fun f (x, y) = x + y
fun f' () = raise Fail ""

structure C = SMLofNJ.Cont

(* val x = C.reset (fn r => *)
(*   C.shift (r, fn k => (k 0) handle Fail _ => 1) + (raise Fail "fail") *)
(* ) handle Fail _ => 2 *)
(* val capture = C.callcc *)
(* val escape = C.throw *)
(* val capture = C.capture *)
(* val escape = C.escape *)
(* val x = capture (fn k' => *)
(*   (capture (fn k => (escape k 42 handle _ => escape k' 111)); raise Fail "") handle _ => 222 *)
(* ) *)

(* fun track _ = capture (fn k => escape k () handle e => raise e) *)

(* fun f () = *)
(*   let val _ = track() *)
(*   in  raise Fail "" *)
(*   end *)

(* val x = *)
(*   let val f = C.callcc (fn k => fn x => C.throw k (fn _ => x)) *)
(*   in  f 1; f true *)
(*   end *)

val x = C.reset (fn r =>
  C.shift (r, fn k => (k 0) handle Fail _ => 1) + (raise Fail "fail")
) handle Fail _ => 2

(* val throwK: bool C.cont option ref = ref NONE *)
(* val b = capture (fn k => (throwK := SOME k; true)) *)
(* val _ = if b then () else raise Fail "" *)
(* val x = capture (fn k' => *)
(*   (escape (valOf (!throwK)) false handle _ => escape k' 111) handle _ => 222 *)
(* ) *)

