fun f x = x;

structure C = SMLofNJ.Cont

structure Control2 :> sig
  type 'a context
  val reset: ('a context -> 'a) -> 'a
  val shift: 'a context * (('b -> 'a) -> 'a) -> 'b
end = struct
  val callcc = SMLofNJ.Cont.callcc
  val throw  = SMLofNJ.Cont.throw
  val isolate  = SMLofNJ.Cont.isolate
  structure Escape :> sig
    type void
    val coerce : void -> 'a
    val escape : (('a -> void) -> 'a) -> 'a
  end = struct
    datatype void = Void of void
    fun coerce (Void v) = coerce v
    fun escape f =
      callcc (fn k => f (fn x => throw k x))
  end
  open Escape

  type 'a context = int
  val counter = ref (1: int)

  val mk : (Unsafe.Object.object * int -> void) ref =
    (* Unsafe.cast (ref (fn _ => throw (!Unsafe.topLevelCont) ())) *)
    (ref (fn _ => raise Fail "top"))

  exception SkippedContext

  fun return (x: Unsafe.Object.object, ctx: 'a context): 'a = coerce (!mk (x, ctx))

  fun reset (thunk: 'a context -> 'a): 'a =
    let val r: 'a context = !counter before counter := !counter + 1
    in  escape (fn k =>
            let val m: (Unsafe.Object.object * 'a context -> void) = !mk
                fun mk' (result: Unsafe.Object.object, ctx: 'a context) =
                  (if r = ctx then (mk := m; k (Unsafe.cast result)) else raise SkippedContext)
                val () = mk := mk'
            in  return (Unsafe.Object.toObject (thunk r), r)
            end)
    end

  fun reset' (ctx: 'a context) (thunk: unit -> 'a): 'a =
    escape (fn k =>
        let val m: (Unsafe.Object.object * 'a context -> void) = !mk
            fun mk' (result: Unsafe.Object.object, ctx: 'a context) =
              (mk := m; k (Unsafe.cast result))
            val () = mk := mk'
        in  return (Unsafe.Object.toObject (thunk ()), ctx)
        end)
    

  fun shift (r: 'ans context, f: ('hole -> 'ans) -> 'ans): 'hole =
    let fun c thunk =
          escape (fn k =>
            let val m = !mk
                fun mk' (result: Unsafe.Object.object, ctx: 'a context) =
                  (if r = ctx then (mk := m; k (Unsafe.cast result)) else raise SkippedContext)
                val () = mk := mk'
            in  return (Unsafe.Object.toObject (thunk ()), r)
            end)
    in  escape (fn k =>
          return (Unsafe.Object.toObject (f (fn v => c (fn () => coerce (k v)))), r))
    end
end
structure C = Control2

(* datatype ('a, 'b) ret = Normal of 'a *)
(*                       | Throw of 'b *)
datatype 'a ret = Normal of 'a | Throw of 'a ret -> 'a ret
exception Type

fun normal (Normal x) = x
  | normal (Throw _) = raise Type

fun append r [] = C.shift (r, fn k => Throw k)
  | append r (a :: rest) =
      (case append r rest
         of Normal rst => Normal (a :: rst)
          | Throw _ => raise Type)

fun append' xs ys =
  (case C.reset (fn r => append r xs)
     of Normal lst => lst
      | Throw k =>
          case k (Normal ys)
            of Normal lst => lst
             | Throw _ => raise Type)

(* datatype 'a ret = Normal of 'a | Throw of 'a ret -> 'a ret *)
(* fun compose (f: 'b -> 'c) (g: 'a -> 'b): 'a -> 'c = *)
(*   C.reset (fn r => f (g (normal (C.shift (r, fn k => Throw (k: 'a ret -> 'a ret)))))) *)
signature CONTROL =
sig
  type ans
  val shift : (('a -> ans) -> ans) -> 'a
  val reset : (unit -> ans) -> ans
end

structure Control :> CONTROL where type ans = unit =
struct
  structure Cont = SMLofNJ.Cont

  type ans = unit

  exception MissingReset

  val mk : (ans Cont.cont) ref = ref (Cont.isolate (fn _ => raise MissingReset))

  fun return x = Cont.throw (!mk) x

  fun reset th = Cont.callcc (fn k => let
        val m = !mk
        in
          mk := Cont.isolate (fn r => (mk := m; Cont.throw k r));
          return (th ())
        end)

  fun shift f = Cont.callcc (fn k => let
        val x = f (fn v => reset (fn () => Cont.throw k v))
        in
          Cont.throw (!mk) x
        end)
end
structure Control' = Control

structure NonDet1 :> sig
  type context
  val choose: context -> 'a list -> 'a
  val run: (context -> unit) -> unit
end = struct
  type context = unit C.context
  fun choose ctx xs = C.shift (ctx, fn k => List.app k xs)
  fun run m = C.reset m
end

structure NonDet2 :> sig
  type context
  val choose: context -> int list -> int
  val run: (context -> unit) -> unit
end = struct
  type context = unit
  fun choose _ xs = Control'.shift (fn k => List.app k xs)
  fun run m = Control'.reset m
end

structure NonDet = NonDet2
val choices = List.tabulate(150, fn x => x)

fun sat r =
  let val choose = NonDet.choose r
      val n = choose choices +
      (* choose choices + *)
      (* choose choices + *)
      (* choose choices + *)
      (* choose choices + *)
      (* choose choices + *)
      (* choose choices + *)
      choose choices +
      choose choices +
      choose choices
  in  f n; ()
    (* print (Int.toString n ^ "\n") *)
    (* print ( *)
    (*     Bool.toString b1 ^ " " ^ Bool.toString b2 ^ " " ^ Bool.toString b3 ^ " " ^ "\n" *)
    (*   ) *)
  end
(* val () = NonDet.run sat *)


structure Eff :> sig
  type void
  val coerce : void -> 'a
  datatype 'e eff = Val | Eff of 'e
  val handleit :
    ('a eff C.context -> unit) ->    (* body *)
    (unit -> unit) ->    (* val *)
    (('a eff -> unit) -> 'a -> unit) (* handler *)
    -> unit
end = struct
  datatype void = Void of void
  datatype 'eff eff = Val | Eff of 'eff
  fun coerce (Void v) = coerce v
  val _ = coerce: void -> 'a

  fun handleit
    (f: 'a eff C.context -> unit)
    (valh: unit -> unit)
    (oph: ('a eff -> unit) -> 'a -> unit): unit =
    let fun loop (e: 'a eff): unit =
          case e
            of Val => valh ()
             | Eff eff => oph loop eff
    in  loop (C.reset (fn (r: 'a eff C.context) => (f r; Val)))
    end
end

structure NonDet :> sig
  type 'a context
  val run : ('a context -> unit) -> unit
  val choose : 'a context -> 'a list -> 'a
  val fail : 'a context -> unit -> Eff.void
end = struct
  open Eff
  datatype 'a nondet
    = Fail of unit * (void -> 'a nondet eff)
    | Choose of 'a list * ('a -> 'a nondet eff)

  type 'a context = 'a nondet eff C.context

  fun choose p arg =
    C.shift (p, fn k => Eff (Choose (arg, k)))
  val _ = choose : 'a nondet eff C.context -> 'a list -> 'a

  fun fail p arg =
    C.shift (p, fn k => Eff (Fail (arg, k)))
  val _ = fail : 'a nondet eff C.context -> unit -> void

  fun run (f: 'a nondet eff C.context -> unit): unit =
    let fun handler loop (Choose (xs, k)) = List.app (loop o k) xs
          | handler loop (Fail ((), _)) = ()
    in  handleit f Fn.id handler
    end
end

(* val test1 = NonDet.run (fn p => *)
(*   let val  x = NonDet.choose p ["a", "b"] *)
(*       val () = print ("1: " ^ x ^ "\n") *)
(*       val y = NonDet.choose p ["c", "d"] *)
(*       val () = print ("2: " ^ y^ "\n") *)
(*   in  app print ["x=", x, " y=", y, "\n"] *)
(*   end) *)

(* signature CONTROL = sig *)
(*   type ans *)
(*   exception Reset *)

(*   val shift : (('a -> ans) -> ans) -> 'a *)
(*   val reset : (unit -> ans) -> ans *)
(* end *)

(* functor Control0(type ans) :> CONTROL where type ans = ans = struct *)
(*   type 'a cont = 'a SMLofNJ.Cont.control_cont *)
(*   val callcc = SMLofNJ.Cont.capture *)
(*   val throw  = SMLofNJ.Cont.escape *)
(*   val isolate  = SMLofNJ.Cont.isolate *)
(*   type ans = ans *)
(*   exception Reset *)
(*   val mk : ans C.control_cont ref = ref (isolate (fn _ => raise Reset)) *)

(*   fun return x = coerce (!mk x) *)

(*   fun reset (thunk: unit -> ans): ans = *)
(*     escape (fn k => let val m = !mk *)
(*                     in  mk := (fn r => (mk := m; k r)); *)
(*                         return (thunk ()) *)
(*                     end) *)

(*    fun shift f = *)
(*      escape (fn k => return (f (fn v => reset (isolate (fn () => coerce (k v)))))) *)
(* end *)


structure Coro :> sig
  type 'a context
  val spawn: 'a context -> ('a context -> unit) -> 'a
  val terminate: 'a context -> 'a -> 'b
  val yield: 'a context -> unit -> unit
  val run : (unit context -> unit) -> unit
end = struct
  open Eff
  datatype 'a co
    = Spawn of ('a co eff C.context -> unit) * ('a -> 'a co eff)
    | Yield of unit * (unit -> 'a co eff)
    | Terminate of 'a * (void -> 'a co eff)

  type 'a context = 'a co eff C.context

  val runnable: (unit -> unit) Queue.queue = Queue.mkQueue ()
  fun suspend f = Queue.enqueue (runnable, f)
  fun restart () =
    case Queue.next runnable
      of NONE => ()
       | SOME f => f ()

  fun spawn p f = C.shift (p, fn k => Eff (Spawn (f, k)))
  val _ = spawn : 'a co eff C.context -> ('a co eff C.context -> unit) -> 'a

  fun terminate p x = coerce (C.shift (p, fn k => Eff (Terminate (x, k))))
  val _ = terminate : 'a co eff C.context -> 'a -> 'b

  fun yield p () = C.shift (p, fn k => Eff (Yield ((), k)))
  val _ = yield : 'a co eff C.context -> unit -> unit

  fun run (f: unit co eff C.context -> unit) =
    let fun handler loop (Spawn (f, k)) = (suspend (loop o k); run f)
          | handler loop (Yield ((), k)) = (suspend (loop o k); restart ())
          | handler loop (Terminate (x, _)) = restart ()
        fun next () = restart ()
    in  handleit f next handler
    end

end

fun process p name count =
  let fun lp n =
        if n > count then
          ()
        else
          (app print [name, " ", Int.toString n, "\n"];
           Coro.yield p ();
           lp (n + 1))
  in  lp 1
  end

val _ = Coro.run (fn p =>
  (Coro.spawn p (fn p => process p "A" 5);
   Coro.spawn p (fn p => process p "B" 3);
   process p "C" 6))

