(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers a reference interpreter for LR(1) automata. It uses the
   LR engine in [MenhirLib.Engine], which it instantiates with the description
   of the grammar and automaton offered by the module [Lr1]. *)

open MiddleAPI

module Make (Lr1 : LR1) : sig
open Lr1.Lr0.G

  (**[interpret strategy show count nt lexer lexbuf] runs the parser, starting
     with the start non-terminal symbol [nt], with the lexer [lexer] and the
     lexing buffer [lexbuf]. It either succeeds and returns [Some cst], where
     [cst] is a concrete syntax tree, or fails and returns [None].

     The parameter [strategy] is the error handling strategy. See
     [IncrementalEngine] for an explanation of the available strategies.

     The parameter [show] determines whether log messages are emitted; it
     corresponds to [--trace]. The parameter [count] is incremented with the
     number of log messages that are emitted or that would be emitted if
     [show] was [true]. *)
  val interpret:
    [`Legacy | `Simplified] ->
    bool -> int ref ->
    Nonterminal.t ->
    (Lexing.lexbuf -> Terminal.t) ->
    Lexing.lexbuf ->
    CST.cst option

  (* [check_error_path], a variant of the reference interpreter, is used
     internally by us. We use it to debug [LRijkstra]. It checks that a
     sentence leads to a syntax error in the expected state. It is also used
     by several of the command line options [--interpret-error],
     [--compile-errors], etc. *)

  (**This type describes the outcome of {!check_error_path}. *)
  type check_error_path_outcome =
  | OInputReadPastEnd
    (**Bad: the input was read past its end. *)
  | OInputNotFullyConsumed
    (**Bad: a syntax error occurred before all of the input was read. *)
  | OUnexpectedAccept
    (**Bad: the parser unexpectedly accepted (part of) this input. *)
  | OK of target
    (**Good: a syntax error occurred after reading the last input token. We
       report in which state the error took place, as well as a list of
       spurious reductions. A non-default reduction that takes place after
       looking at the last input token (i.e., the erroneous token) is
       spurious. A spurious reduction can take place only in a non-canonical
       LR automaton. *)

  and target =
    Lr1.node * spurious_reduction list

  and spurious_reduction =
    Lr1.node * Production.t

  (**[check_error_path strategy show count nt input] runs the parser, starting
     with the start non-terminal symbol [nt], with the input stream [input].
     We expect [input] to be a minimal invalid input: the parser is expected
     to fail after reading all of [input].

     The parameter [strategy] is the error handling strategy. See
     [IncrementalEngine] for an explanation of the available strategies.

     The parameter [show] determines whether log messages are emitted; it
     corresponds to [--trace]. The parameter [count] is incremented with the
     number of log messages that are emitted or that would be emitted if
     [show] was [true]. *)
  val check_error_path:
    [`Legacy | `Simplified] ->
    bool -> int ref ->
    Nonterminal.t ->
    Terminal.t list ->
    check_error_path_outcome

end
