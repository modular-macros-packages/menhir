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
(X : sig

  (**If [mode] is [`DefineTokenType] then the types [token] and ['a terminal]
     are defined as algebraic data types. If it is [`UseExternalTokenType "M"]
     then they are defined as abbreviations for pre-existing types [M.token]
     and ['a M.Interpreter.terminal]. *)
  val mode: [ `DefineTokenType | `UseExternalTokenType of string]

end)
: sig
  val unparsing_API : unit -> structure
end

module Interface (G : GRAMMAR)
: sig
  val unparsing_API : unit -> interface
end

val unparsing_API : PlainSyntax.grammar -> interface
