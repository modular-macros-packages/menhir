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

type target =
  Lr1.node * spurious_reduction list

and spurious_reduction =
  Lr1.node * Production.t

(**[print_messages_item] displays one data item. The item is of the form [nt,
   sentence, target], which means that beginning at the start symbol [nt], the
   sentence [sentence] ends in an error in the target state given by [target].
   [target] also contains information about which spurious reductions are
   performed at the end. The display obeys the [.messages] file format. *)
val print_messages_item:
  Nonterminal.t * Terminal.t list * target -> unit

exception EndOfStream

(**[stream] turns a finite list of terminals into a stream of terminals,
   represented as a pair of a lexer and a lexing buffer, so as to be usable
   with Menhir's traditional API. This lexer can raise [EndOfStream]. *)
val stream: Terminal.t list -> (Lexing.lexbuf -> Terminal.t) * Lexing.lexbuf

type filename =
  string

(* TODO comment *)
module Command : sig
  val interpret: [`ShowCST | `DoNotShowCST] -> unit
  val interpret_error: unit -> unit
  val compile_errors: filename -> unit
  val compare_errors: filename -> filename -> unit
  val merge_errors: filename -> filename -> unit
  val update_errors: filename -> unit
  val echo_errors: filename -> unit
  val echo_errors_concrete: filename -> unit
end
