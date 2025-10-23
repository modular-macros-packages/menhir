(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module implements the elimination of [%inline] nonterminal symbols. *)

open PlainSyntax

module Make (E : sig

  (**[error ranges format ...] reports an error and aborts. *)
  val error : ranges -> ('a, out_channel, unit, 'b) format4 -> 'a

  (**[warning ranges format ...] reports a warning. It does not abort. *)
  val warning: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

end) : sig

  (**[inline g] traverses the grammar [g] and inlines away the nonterminal
     symbols whose definitions are marked [%inline]. The result is a grammar
     where no symbols are marked [%inline]. *)
  val inline: grammar -> grammar

end
