(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module implements severe conflict resolution. That is, all of the
   shift/reduce and reduce/reduce conflicts that exist in the automaton (which
   could not be silently solved) are resolved in a default, possibly arbitrary
   manner. No transitions are removed. Some reductions are removed. *)

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

end)
: sig

  include LR1_AUTOMATON with module Lr0 = A.Lr0

  (**[diagnostics()] emits a warning if at least one severe conflict
     has been resolved. *)
  val diagnostics: unit -> unit

end
