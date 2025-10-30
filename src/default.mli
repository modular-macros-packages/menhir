(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Grammar

(**[test_default_reduction node] returns [Some (prod, ts)] if the node [node]
   has a default reduction of the production [prod] on the set set of
   terminal symbols [ts]. It returns [None] if this node has no default
   reduction.

   When the result is [Some (prod, ts)], this implies that the node [node]
   has a reduction of [prod] on every symbol in the set [ts], and does not
   have any other shift or reduce actions. The reduction of [prod] is then
   considered a default reduction: this reduction is performed without
   inspecting the next input symbol and sometimes even without requesting
   the next input symbol.

   If end-of-stream conflicts have been resolved then the set [ts] is either
   a singleton set containing just the terminal symbol [#] or a set of
   non-[#] terminal symbols. In the former case, reduction is performed
   without even requesting the next input token. In the latter case, first
   the next input token is requested, then reduction is performed, without
   inspecting this input token. *)
val test_default_reduction : Lr1.node -> (Production.t * TerminalSet.t) option

(* [find_erroneous s zs] finds whether there is a terminal [z] in [zs] such
   that state [s] will initiate an error on the lookahead symbol [z].
   All terminals in [zs] must be real. *)

val find_erroneous: Lr1.node -> TerminalSet.t -> Terminal.t option
