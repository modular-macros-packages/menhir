(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Syntax

module Make (Error : sig

  (**[error ranges format ...] reports an error and aborts. *)
  val error : ranges -> ('a, out_channel, unit, 'b) format4 -> 'a

  (**[note ranges format ...] emits an information message. It does not abort.*)
  val note: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

end) : sig

  (**[join] combines several partial grammars to form a single grammar. *)
  val join : partial_grammar list -> grammar

end
