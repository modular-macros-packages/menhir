(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(***)

open MiddleAPI

module Run
(A : LR1_AUTOMATON)
(E : sig

  (**[warning ranges format ...] reports a warning. It does not abort. *)
  val warning: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

  (**[log format ...] emits an information message. *)
  val log: ('a, out_channel, unit, unit) format4 -> 'a

end)
: sig

  include LR1_AUTOMATON with module Lr0 = A.Lr0
                         and type node = A.node

  (**[diagnostics()] emits a warning if at least one severe conflict
     has been resolved. *)
  val diagnostics: unit -> unit

end
