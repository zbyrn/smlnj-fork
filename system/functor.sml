(
  Control.progressMsgs := true
  ; Control.FLINT.rtdebugging := true
  ; Control.FLINT.redebugging := true
  ; Control.FLINT.printFlint := true
  ; Control.FLINT.printPhases := true
  ; Control.FLINT.printAllIR := true
  ; Control.CG.printit := true
  ; Control.Print.printDepth := 10000000
  ; Control.Print.lineWidth := 150
);

signature SIG = sig
  val f : 'a -> 'a
  type t
  val x : t
end

functor F(S: SIG): sig type t val result : t end where type t = S.t = struct
  open S
  val result = f f x
end
