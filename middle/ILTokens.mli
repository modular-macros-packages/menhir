(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module helps generate IL code that deals with tokens. *)

open IL
open MiddleAPI

module Make (G : GRAMMAR) (X : sig

  (**If [mode] is [`DefineTokenType] then the types [token] and ['a terminal]
     are defined as algebraic data types. If it is [`UseExternalTokenType "M"]
     then they are defined as abbreviations for pre-existing types [M.token]
     and ['a M.Interpreter.terminal]. *)
  val mode: [ `DefineTokenType | `UseExternalTokenType of string]

end) : sig
  open G

  (**[tokpat t pat] is a pattern that matches a token for the terminal symbol
     [t] and binds its semantic value (if it has one) to the pattern [pat]. *)
  val tokpat: Terminal.t -> pattern -> pattern

  (**[tokexpr t e] is an expression that constructs a token for the terminal
     symbol [t], taking its semantic value (if it needs one) from the
     expression [e]. *)
  val tokexpr: Terminal.t -> expr -> expr

  (**[tokspat ts] is a pattern that matches a token for any terminal symbol in
     the set [ts]. The semantic value is not bound. *)
  val tokspat: TerminalSet.t -> pattern

  (**If the terminal symbol [t] has no semantic value then [tok_bind_unit t
     pat e] binds the pattern [pat] to the unit value in the expression [e] .
     If [t] has a semantic value then it expands to just [e]. *)
  val tok_bind_unit: Terminal.t -> pattern -> expr -> expr

end
