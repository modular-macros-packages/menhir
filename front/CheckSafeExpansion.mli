(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers a termination test for the expansion of parameterized
   nonterminal symbols. *)

open Syntax

module Make (E : sig

  (**[error ranges format ...] reports an error and aborts. *)
  val error : ranges -> ('a, out_channel, unit, 'b) format4 -> 'a

end) : sig

  (**[check] accepts a parameterized grammar, where all parameters must have
     sort [*]. (Parameters of higher sort must be eliminated prior to running
     this test: see [SelectiveExpansion].) The test succeeds if and only if
     the expansion of this grammar is safe, that is, expansion terminates. *)
  val check: grammar -> unit

end
