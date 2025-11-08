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
open MiddleAPI

module Code (G : GRAMMAR)
(A : sig
  (* A list of the start symbols and corresponding start states. *)
  val entry: (G.Nonterminal.t * int) list
end)
(N : sig
  val tables: string
end)
: sig
  val unparsing_API : unit -> structure
end

module Interface (G : GRAMMAR)
: sig
  val unparsing_API : unit -> interface
end

val unparsing_API : PlainSyntax.grammar -> interface
