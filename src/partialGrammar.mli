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

  (**[warning ranges format ...] reports a warning. It does not abort. *)
  val warning: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

end) (X : sig

  (**[ignore_unused_token t] returns [true] if an ignored-token warning
     about the token [t] should be suppressed. *)
  val ignore_unused_token : terminal -> bool

end) : sig

  (**[join] combines several partial grammars to form a single grammar. *)
  val join : partial_grammar list -> grammar

end
