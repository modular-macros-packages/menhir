(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module transforms an LR(1) automaton by introducing default
   reductions. End-of-stream conflicts must have already been resolved
   by running [SevereConflictResolution]. *)

open Report
open MiddleAPI

module Run (A : LR1_AUTOMATON) : sig

  include LR1_AUTOMATON with module Lr0 = A.Lr0
                         and type node = A.node

  (**[diagnostics c] logs a message on the channel [c] indicating how
     many default reductions have been introduced. *)
  val diagnostics: channel -> unit

end
