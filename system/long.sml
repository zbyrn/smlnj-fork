(* ( *)
(*   Control.progressMsgs := true *)
(*   ; Control.FLINT.rtdebugging := true *)
(*   ; Control.FLINT.redebugging := true *)
(*   ; Control.FLINT.printFlint := true *)
(*   ; Control.FLINT.printPhases := true *)
(*   ; Control.FLINT.printAllIR := true *)
(*   ; Control.FLINT.check := true *)
(*   ; Control.FLINT.checkKinds := true *)
(*   ; Control.CG.printit := true *)
(*   ; Control.Print.printDepth := 10000000 *)
(*   ; Control.Print.lineWidth := 150 *)
(* ); *)
fun I x = x;
fun f x = I I I I I I I I I I I I I I I I I I I x;
(* fun f x = I I I I I I I I I x; *)

fun If (r: real * real * real) = r;

(* val f = I I I I I If *)
(* fun f x = I I I x; *)
(* fun f x = I (I (I (I (I (I x))))); *)

(* val y = f (1.0, 2.0, 3.0) *)
(* val y = f (1, 2) *)
(* val y = f 3 *)

(* fun apply (f, x) = f x; *)

(* fun If (x : real * real) = x; *)
(* fun I (x: int * int) = x; *)

(* val x = apply (If, (1.0, 2.0)); *)
(* val x = apply (I, (1, 2)); *)

val _ =
  let val (r1, r2, r3) = f (1, 2, 3)
  in  print (Int.toString r1)
  end
