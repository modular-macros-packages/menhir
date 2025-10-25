(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module contains the logic that decides how conflicts should be
   resolved. It also offers a mechanism to detect and report precedence
   declarations and %prec annotations that do not help resolve any
   conflicts. This mechanism involves mutable state, which is allocated
   and initialized when [Make] is applied. *)

(* [Make] requires both the grammar in [PlainSyntax] format and a view of
   the grammar as several modules [Terminal], [Nonterminal], etc. The
   grammar [grammar] is necessary because the list [grammar.tokens] includes
   the pseudo-tokens (that is, the names that appear in %prec annotations).
   This information is not accessible elsewhere. *)

open PlainSyntax
open View

module Make
(E : sig

  (**[warning ranges format ...] reports a warning. It does not abort. *)
  val warning: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

end)
(F : sig
  val grammar : grammar
end)
(G : GRAMMAR)
: sig
open G

  (**A choice indicates how a shift/reduce conflict should be resolved. *)
  type choice =
    | ChooseShift
    | ChooseReduce
    | ChooseNeither
    | DontKnow

  (**A shift/reduce conflict requires making a choice between shifting a token
     [tok] and reducing a production [prod].

     Shifting is preferred if the token has higher precedence than the
     production, or if they have the same precedence and the token is
     right-associative.

     Reducing is preferred if the token has lower precedence than the
     production, or if they have the same precedence and the token is
     left-associative.

     Neither is allowed when the token and the production have the same
     precedence and the token is non-associative.

     No choice is made if the token or the production has undefined
     precedence. In that case, the default choice is to prefer shifting,
     but a conflict is reported.

     [shift_reduce tok prod] determines how a shift/reduce conflict between
     the token [tok] and the production [prod] should be resolved. *)
  val shift_reduce: Terminal.t -> Production.index -> choice

  (**A reduce/reduce conflict requires making a choice between reducing two
     distinct productions.

     This choice is made by exploiting a partial order on productions. For
     compatibility with ocamlyacc, this order should be total and should
     correspond to textual order when the two productions originate in the
     same source file. When they originate in different source files, the two
     productions are considered incomparable.

     [reduce_reduce prod1 prod2] determines how a reduce/reduce conflict
     between the productions [prod1] and [prod2] should be resolved. *)
  val reduce_reduce:
    Production.index -> Production.index -> Production.index option

  (**[diagnostics()] should be called after the automaton has been constructed,
     that is, after all calls to {!shift_reduce} have taken place. This function
     prints warnings about useless precedence declarations for terminal symbols
     (%left, %right, %nonassoc) and productions (%prec). *)
  val diagnostics: unit -> unit

end
