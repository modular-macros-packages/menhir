(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module implements silent conflict resolution. The LR(1) automaton is
   transformed by obeying user-provided precedence declarations. Some
   transitions can be removed. This can make some states unreachable, but the
   states are not renumbered; all states are retained. Some reductions can be
   removed. Some default reductions can become forbidden. *)

open MiddleAPI

module Run
(A : LR1_AUTOMATON)
(Precedence : sig
  open A.Lr0.G

  (**A choice indicates how a shift/reduce conflict should be resolved. *)
  type choice =
    | ChooseShift
    | ChooseReduce
    | ChooseNeither
    | DontKnow

  (**[shift_reduce t prod] determines how a shift/reduce conflict between the
     terminal symbol [t] and the production [prod] should be resolved. *)
  val shift_reduce: Terminal.t -> Production.t -> choice

end)
(E : sig

  (**[warning ranges format ...] reports a warning. It does not abort. *)
  val warning: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

  (**[log format ...] emits an information message. *)
  val log: ('a, out_channel, unit, unit) format4 -> 'a

end)
: sig

  include LR1_AUTOMATON with module Lr0 = A.Lr0

  (**[diagnostics()] emits an information message about the conflicts that
     have been silently solved and warnings about the severe conflicts that
     remain. *)
  val diagnostics: unit -> unit

end
