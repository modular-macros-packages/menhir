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

(* ------------------------------------------------------------------------ *)

(* Items. *)

(* An LR(0) item encodes a pair of integers, namely the index of the
   production and the index of the bullet in the production's right-hand
   side. *)

(* Both integers are packed into a single integer, using 10 bits for
   the bullet position and the rest (21 bits on a 32-bit architecture,
   53 bits on a 64-bit architecture) for the production index. This
   means that the length of a production must be at most 1023. This
   means that the number of productions must be at most:
   - 2^21, that is about 2 million, on a 32-bit architecture;
   - 2^53, that is practically unlimited, on a 64-bit architecture. *)

(* These static limits could be adjusted if necessary. It would also be
   possible to dynamically adjust the limits depending on the grammar
   at hand. In practice, the need for this has not been felt. *)

(* WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING *)

(* The constants [low_bits], [low_limit] and the function [export] are
   duplicated in [lib/InspectionTableInterpreter.ml] and in
   [front/CheckGrammar.ml]. Do not modify them at all, or modify them
   everywhere in a consistent manner. *)

type t = int

let low_bits =
  10 (* have you read the above warning? *)

let low_limit =
  1 lsl low_bits

let equal =
  Int.equal

let[@inline] import (prod, pos) =
  assert (0 <= pos && pos <= Production.length prod);
  assert (pos < low_limit);
  (Production.p2i prod) lsl low_bits + pos

let[@inline] export item =
  Production.i2p (item lsr low_bits),
  item mod low_limit

let[@inline] marshal item =
  item

let is_start item =
  let prod, pos = export item in
  Production.is_start prod && pos = 0

let test_start item =
  let prod, _pos = export item in
  Production.test_start prod

let print item =
  let prod, pos = export item in
  let nt = Production.nt prod in
  let rhs = Production.rhs prod in
  Printf.sprintf "%s -> %s"
    (Nonterminal.print false nt)
    (Symbol.print_array_bullet false pos rhs)

type kind =
  | Shift of Symbol.t * t
  | Reduce of Production.t

let classify item =
  let prod, pos = export item in
  let rhs = Production.rhs prod in
  let n = Array.length rhs in
  if pos = n then
    Reduce prod
  else
    Shift (rhs.(pos), import (prod, pos + 1))

(* Sets of items and maps over items. Hashing these data structures is
   specifically allowed, so balanced trees (for instance) would not be
   applicable here. *)

(* The main reason why [Set] is implemented using Patricia trees, as
   opposed to bit sets, is that, given our current encoding of items,
   items are not small consecutive integers. *)

module Map =
  IntMap.Unbounded

module Set =
  Map.Domain
