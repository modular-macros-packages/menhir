(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Grammar

(**An LR(0) item encodes a pair of integers, namely the index of the
   production and the index of the bullet in the production's right-hand
   side. *)
type t

(**[equal] determines whether two items are equal. *)
val equal: t -> t -> bool

(**[import (prod, i)] converts a pair of a production [prod] and an index [i]
   to an item. The index [i] must lie in the interval [\[0, n\]], where [n]
   is the length of production [prod]. *)
val import: Production.t * int -> t

(**[export item] converts the item [item] back to a pair of a production
   [prod] and an index [i]. The index [i] lies in the interval [\[0, n\]],
   where [n] is the length of production [prod]. *)
val export: t -> Production.t * int

(**An item is internally encoded as a single integer value. This feature
   should be used as little as possible. It is currently exploited in the
   table back-end only. The decoding function (which is really a copy of
   [export]) is in [InspectionTableInterpreter]. *)
val marshal: t -> int

(**[is_start item] determines whether the item [item] is a start item.
   This is the case if the production [prod] is a start production and
   the index [i] is zero. *)
val is_start: t -> bool

(**[test_start item] determines whether the item [item] is a start item.
   If so, the user start symbol that corresponds to this start production
   is returned. *)
val test_start: t -> Nonterminal.t option

(* Printing. *)

val print: t -> string

(* Classifying items as shift or reduce items. A shift item is one
   where the bullet can still advance. A reduce item is one where the
   bullet has reached the end of the right-hand side. *)

type kind =
  | Shift of Symbol.t * t
  | Reduce of Production.t

val classify: t -> kind

(* Sets of items and maps over items. Hashing these data structures is
   specifically allowed. *)

module Map : API.MAP_AND_SET with type key = t

module Set = Map.Domain

(* This functor performs precomputation that helps efficiently compute
   the closure of an LR(0) or LR(1) state. The precomputation requires
   time linear in the size of the grammar. The nature of the lookahead
   sets remains abstract. *)

module Closure (L : Lookahead.S) : sig

  (* A state maps items to lookahead information. *)

  type state = L.t Map.t

  (* This takes the closure of a state through all epsilon transitions. *)

  val closure: state -> state

end
