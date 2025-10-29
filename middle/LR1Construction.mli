(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module constructs an LR(1) automaton using one of four methods, which
   differ in the amount of fusion of states that they perform. It is a wrapper
   for the three modules [LR1Canonical], [LR1Pager], and [LALR]. *)

open MiddleAPI

module Make (Lr0 : LR0)
(X : sig

  (**The parameter [mode] determines which construction method is used.
     In short,
     - Knuth's canonical LR(1) construction performs no fusion.
       This is the minimal amount of fusion.
     - In "inclusion-only" mode, two states are fused only
       if one is a subset of the other.
     - In "Pager" mode, two states are fused if they compatible
       according to a variant of Pager's weak compatibility criterion.
     - In LALR mode, all states with the same LR(0) core must be fused.
       This is the maximal amount of fusion. *)
  val mode: [`Canonical | `InclusionOnly | `Pager | `LALR]

end) ()
: LR1_AUTOMATON with module Lr0 = Lr0
