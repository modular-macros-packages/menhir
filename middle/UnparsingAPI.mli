(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module generates the code and the signature of the unparsing API. *)

open IL
open PlainSyntax
open FrontAPI
open MiddleAPI

module Code
(G : GRAMMAR)
(A : sig

  (**[entry] is a list of the start symbols and corresponding start states. *)
  val entry: (G.Nonterminal.t * int) list

end)
(N : sig

  (**[tables] is the name of the module that contains the parse tables. *)
  val tables: string

end)
(X : TOKEN_TYPE_SETTINGS)
: sig

  (**[unparsing_API] is the code of the unparsing API. *)
  val unparsing_API : unit -> structure

end

(**[unparsing_API] is the functor {!Interface}, re-packaged as a function. *)
val unparsing_API : grammar -> interface
