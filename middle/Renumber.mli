(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module discovers the reachable states of an LR(1) automaton and
   renumbers them in a consecutive manner. The unreachable states disappear. *)

open MiddleAPI

module Run (A : LR1_AUTOMATON) : sig

  include LR1_AUTOMATON with module Lr0 = A.Lr0

  (**[diagnostics()] emits an information message about the conflicts that
     have been silently solved and warnings about the severe conflicts that
     remain. *)
  (* val diagnostics: unit -> unit *)

end
