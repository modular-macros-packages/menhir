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

module Make (E : sig

  (**[can_complain_about symbol] determines whether it is permitted to report
     an error, using [signal], about the symbol [symbol]. This function must
     return [true] if queried about a certain symbol for the first time, and
     can return [false] if queried again about the same symbol. This lets us
     avoid flooding the user with multiple reports about a single symbol. *)
  val can_complain_about : symbol -> bool

  (**[error ranges format ...] reports an error and aborts. *)
  val error : ranges -> ('a, out_channel, unit, 'b) format4 -> 'a

  (**[signal ranges format ...] reports an error, but does not abort.
     Therefore, it is possible to report several errors. *)
  val signal: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

  (**[note ranges format ...] emits an information message. It does not abort.*)
  val note: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

end) : sig

  (**[join] combines several partial grammars to form a single grammar.

     [join] does not raise an exception or abort when an error is signaled.
     It is up to the caller to notice that [signal] has been called and to
     take appropriate action. *)
  val join : partial_grammar list -> grammar

end
