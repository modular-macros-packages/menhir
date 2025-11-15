(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Report
open PlainSyntax

(**[filter_grammar c] warns when the [error] token is used elsewhere than at
   the end of a production. It returns a grammar where the faulty productions
   have been removed. The channel [c] is used to emit warnings. *)
val filter_grammar: channel -> grammar -> grammar

(* Why do we do this? The simplified strategy does not query the lexer for a
   new token after shifting the [error] token. This implies that the [error]
   token remains on the stream. If the [error] token is used at the end of a
   production, then this is not a problem; after we shift, a reduction must
   take place, whose semantic action will abort the parser. However, if the
   [error] token was used inside a production, then we could very well fall
   into an endless shift cycle. *)
