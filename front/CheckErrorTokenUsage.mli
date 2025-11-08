(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open PlainSyntax

module Make (E : sig

  (**[warning ranges format ...] reports a warning. It does not abort. *)
  val warning: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

end) : sig

(**[filter_grammar] warns when the [error] token is used elsewhere than at the
   end of a production. It returns a grammar where the faulty productions have
   been removed. *)
val filter_grammar : grammar -> grammar

(* Why do we do this? The simplified strategy does not query the lexer for a
   new token after shifting the [error] token. This implies that the [error]
   token remains on the stream. If the [error] token is used at the end of a
   production, then this is not a problem; after we shift, a reduction must
   take place, whose semantic action will abort the parser. However, if the
   [error] token was used inside a production, then we could very well fall
   into an endless shift cycle. *)

end
