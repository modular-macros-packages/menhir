(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module computes which states are able to reduce each production
   and warns about productions that can never be reduced. *)

open MiddleAPI

module Run
(A : LR1_AUTOMATON)
(E : sig

  (**[warning ranges format ...] reports a warning. It does not abort. *)
  val warning: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

end)
: sig
  open A.Lr0.G
  open A

  (**[sites prod] is a list of the nodes that are able to reduce
     the production [prod]. *)
  val reduction_sites: Production.t -> node list

  (**[diagnostics()] emits warnings about productions
     that can never be reduced. *)
  val diagnostics: unit -> unit

end
