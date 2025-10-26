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

module Run (A : LR1_AUTOMATON) = struct
open A.Lr0.G

(* -------------------------------------------------------------------------- *)

(* Include the automaton [A]. Although this may seem surprising, almost every
   function in the description of [A] can be retained, unchanged. Only [n],
   [encode], and [decode] must be redefined, so as to get a new numbering of
   nodes. *)

(* All of the data that describes [A] is retained, including the description
   of the unreachable nodes. This is a memory leak, but it is acceptable
   because the unreachable nodes are not numerous. *)

include A

(* -------------------------------------------------------------------------- *)

(* Traverse the transitions of the LR(1) automaton and renumber its nodes. *)

module G = struct
  type t = A.node
  let foreach_root yield =
    ProductionMap.iter (fun _prod node -> yield node) A.entry
  let foreach_successor node yield =
    SymbolMap.iter (fun _symbol node -> yield node) (A.transitions node)
end

include Fix.GraphNumbering.ForNumberedType(A)(G)
    (* type t = A.node *)
    (* n : int *)
    (* encode : A.node -> int *)
    (* decode : int -> A.node *)

end (* Run *)
