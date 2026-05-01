(* wrapunwrap.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure WrapUnwrapCancel : sig
  val cancel : FLINT.prog -> FLINT.prog
end = struct

local
  structure F = FLINT
  structure FU = FlintUtil
  structure LK = LtyKernel
  structure LV = LambdaVar
  structure PO = Primop
  structure PL = PLambda
in

type env = (LV.lvar * F.value) list

fun bind (m: env, v: LV.lvar, x: F.value) = (v, x) :: m

fun kill (m: env, vs: LV.lvar list) =
  List.filter (fn (v, _) => not (List.exists (fn x => x = v) vs)) m

fun look (m: env, v: LV.lvar) =
  case List.find (fn (x, _) => x = v) m
   of SOME (_, y) => SOME y
    | NONE => NONE

fun sv (m: env, x: F.value) =
  case x
   of F.VAR v => (case look (m, v)
                   of SOME y => y
                    | NONE => x)
    | _ => x

fun tysEq (a, b) = LK.tc_eqv(a, b)

fun cancel (prog: F.prog) =
  let
    fun lpfd (m: env, (fk, f, vts, e): F.fundec) =
      let val m' = kill(m, f :: map #1 vts)
       in (fk, f, vts, loop(m', e))
      end

    and lpcon (m: env, c, e) =
      (case c
        of PL.DATAcon(_, _, v) => loop(kill(m, [v]), e)
         | _ => loop(m, e))

    and loop (m: env, le: F.lexp) : F.lexp =
      (case le
        of F.RET vs => F.RET (map (fn x => sv(m, x)) vs)

         | F.LET(vs, e1, e2) =>
             let val ne1 = loop(m, e1)
                 val m2 = kill(m, vs)
                 val ne2 = loop(m2, e2)
              in F.LET(vs, ne1, ne2)
             end

         | F.FIX(fdecs, e) =>
             let val fs = map #2 fdecs
                 val m' = kill(m, fs)
                 val nfdecs = map (fn fd => lpfd(m', fd)) fdecs
              in F.FIX(nfdecs, loop(m', e))
             end

         | F.APP(v, vs) => F.APP(sv(m, v), map (fn x => sv(m, x)) vs)

         | F.TFN((tfk, f, tvks, e1), e2) =>
             let val m' = kill(m, [f])
              in F.TFN((tfk, f, tvks, loop(m', e1)), loop(m', e2))
             end

         | F.TAPP(v, ts) => F.TAPP(sv(m, v), ts)

         | F.SWITCH(v, csig, cases, d) =>
             F.SWITCH(sv(m, v), csig,
                      map (fn (c, e) => (c, lpcon(m, c, e))) cases,
                      Option.map (fn e => loop(m, e)) d)

         | F.CON(dc, ts, u, v, e) =>
             F.CON(dc, ts, sv(m, u), v, loop(kill(m, [v]), e))

         | F.RECORD(rk, vs, v, e) =>
             F.RECORD(rk, map (fn x => sv(m, x)) vs, v, loop(kill(m, [v]), e))

         | F.SELECT(u, i, v, e) =>
             F.SELECT(sv(m, u), i, v, loop(kill(m, [v]), e))

         | F.RAISE(u, ts) => F.RAISE(sv(m, u), ts)

         | F.HANDLE(e, v) => F.HANDLE(loop(m, e), sv(m, v))

         | F.BRANCH(po, vs, e1, e2) =>
             F.BRANCH(po, map (fn x => sv(m, x)) vs, loop(m, e1), loop(m, e2))

         | F.PRIMOP(po1 as (_, p1, _, _), [u1], v1, e1) =>
             (case (p1, e1)
               of (PO.WRAP, F.PRIMOP(po2 as (_, p2, _, _), [F.VAR w], v2, e2)) =>
                    if (w = v1) andalso (p2 = PO.UNWRAP) andalso
                       tysEq(FU.getWrapTyc po1, FU.getUnWrapTyc po2)
                    then F.PRIMOP(po1, [sv(m, u1)], v1,
                                  loop(bind(kill(m, [v2]), v2, sv(m, u1)), e2))
                    else F.PRIMOP(po1, [sv(m, u1)], v1, loop(kill(m, [v1]), e1))
                | (PO.UNWRAP, F.PRIMOP(po2 as (_, p2, _, _), [F.VAR w], v2, e2)) =>
                    if (w = v1) andalso (p2 = PO.WRAP) andalso
                       tysEq(FU.getUnWrapTyc po1, FU.getWrapTyc po2)
                    then F.PRIMOP(po1, [sv(m, u1)], v1,
                                  loop(bind(kill(m, [v2]), v2, sv(m, u1)), e2))
                    else F.PRIMOP(po1, [sv(m, u1)], v1, loop(kill(m, [v1]), e1))
                | _ => F.PRIMOP(po1, [sv(m, u1)], v1, loop(kill(m, [v1]), e1)))

         | F.PRIMOP(po, vs, v, e) =>
             F.PRIMOP(po, map (fn x => sv(m, x)) vs, v, loop(kill(m, [v]), e)))

    val (fk, f, vts, e) = prog
  in
    (fk, f, vts, loop([], e))
  end

end
end
