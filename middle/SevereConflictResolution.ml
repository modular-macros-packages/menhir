(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open MiddleAPI

module Run
(A : LR1_AUTOMATON)
(Precedence : sig
  open A.Lr0.G

  (**[reduce_reduce prod1 prod2] determines how a reduce/reduce conflict
     between the productions [prod1] and [prod2] should be resolved. *)
  val reduce_reduce:
    Production.t -> Production.t -> Production.t option

end)
(E : sig

  (**[warning ranges format ...] reports a warning. It does not abort. *)
  val warning: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

  (**[log format ...] emits an information message. *)
  val log: ('a, out_channel, unit, unit) format4 -> 'a

end)
= struct
include A
open Lr0.G
open E

(* -------------------------------------------------------------------------- *)

let tabulate (f : node -> 'a) : 'a array =
  Array.init A.n @@ fun i ->
  f (A.decode i)

(* -------------------------------------------------------------------------- *)

(* Mutable state. *)

(* These arrays are indexed by node indices. *)

(* The array [_reductions] is initialized here with the data supplied by
   the function [A.reductions]. During the traversal, some reductions
   are removed, because of conflict resolution. *)

let _reductions : Reductions.t array =
  tabulate A.reductions

(* -------------------------------------------------------------------------- *)

(* Read accessors. *)

(* These accessors are public: they are part of the signature [LR1_AUTOMATON].*)

let[@inline] reductions node =
  _reductions.(A.encode node)

(* Write accessors. *)

let[@inline] filter_reductions node f =
  let i = A.encode node in
  _reductions.(i) <- TerminalMap.filter f _reductions.(i)

(* -------------------------------------------------------------------------- *)

(* [best prod prods] chooses which production, out of the nonempty list
   [prod :: prods], should be reduced. If no best choice exists then one
   or more warnings are emitted and an arbitrary choice is made. *)

(* The cause for not knowing which production is best could be:
   1- the productions originate in different source files;
   2- they are derived, via inlining or expansion of parameterized
      non-terminal symbols, from the same production. *)

let rec best candidate = function
  | [] ->
      candidate
  | prod :: prods ->
      match Precedence.reduce_reduce candidate prod with
      | Some candidate ->
          best candidate prods
      | None ->
          let open Production in
          let () =
            warning (positions candidate @ positions prod)
              "do not know how to resolve a reduce/reduce conflict\n\
               between the following two productions:\n%s\n%s"
              (print candidate) (print prod)
          in
          candidate (* arbitrary *)

(* -------------------------------------------------------------------------- *)

(* Diagnostics. *)

let diagnostics () =
  ()

end (* Run *)
