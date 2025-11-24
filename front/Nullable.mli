(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module computates which symbols are nullable. *)

open PlainSyntax
open GenericAnalysis

module Make
(X : sig val grammar: grammar end)
: RESULT
  with type property = bool
   and type nonterminal = nonterminal
   and type terminal = terminal
   and type symbol = symbol
   and type production = branch
