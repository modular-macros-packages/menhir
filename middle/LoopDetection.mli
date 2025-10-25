(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module detects and rejects certain anomalies in the grammar,
   which cause the grammar to be outside of the class LR(1). These
   anomalies include cycles and hidden left recursion. *)

open MiddleAPI

module Run (E : sig

  (**[error ranges format ...] reports an error and aborts. *)
  val error : ranges -> ('a, out_channel, unit, 'b) format4 -> 'a

end)
(G : GRAMMAR)
: sig end
