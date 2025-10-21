(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module runs the grammar functor on the grammar produced by the
   front-end. *)

module  G = GrammarFunctor.Make(Front)()
include G

module Precedence =
  Precedence.Make(Error)(Front)(G)

(* -------------------------------------------------------------------------- *)

(* Print information messages. *)

open Printf

let () =
  Error.logG 1 @@ fun f ->
  let n = Nonterminal.n
  and start = Production.start in
  fprintf f
    "Grammar has %d nonterminal symbols, among which %d start symbols.\n"
    (n - start) start

let () =
  Error.logG 1 @@ fun f ->
  let n = Terminal.n in
  fprintf f "Grammar has %d terminal symbols.\n" (n - 2)

let () =
  Error.logG 1 @@ fun f ->
  let n = Production.n
  and start = Production.start in
  fprintf f "Grammar has %d productions.\n" (n - start)

(* -------------------------------------------------------------------------- *)

(* Based on the result of the analyses NONEMPTY and FIRST, emit warnings and
   errors. *)

open Nonterminal
open Analysis

let errors =
  Error.new_category()

(* If a start symbol generates the empty language or generates the language
   {epsilon}, report an error. In principle, this could be just a warning.
   However, in [Engine], in the function [start], it is convenient to assume
   that neither of these situations can arise. This means that at least one
   token must be read. *)

let () =
  iter_user_start @@ fun nt ->
  if not (nonempty nt) then
    Error.signal errors
      (positions nt)
      "%s generates the empty language." (print false nt);
  if TerminalSet.is_empty (first nt) then
    Error.signal errors
      (positions nt)
      "%s generates the language {epsilon}." (print false nt)

(* If a nonterminal symbol generates the empty language, issue a warning. *)

let () =
  iterx @@ fun nt ->
  if not (nonempty nt) then
    Error.warning
      (positions nt)
      "%s generates the empty language." (print false nt)

let () =
  Error.(exit_if errors)

(* -------------------------------------------------------------------------- *)

(* If requested, dump the results of the analyses. *)

let () =
  Error.logG 2 @@ fun f ->

  iterx (fun nt ->
    fprintf f "nullable(%s) = %b\n"
      (print false nt)
      (nullable nt)
  );
  iterx (fun nt ->
    fprintf f "first(%s) = %s\n"
      (print false nt)
      (TerminalSet.print (first nt))
  );
  iterx (fun nt ->
    fprintf f "minimal(%s) = %s\n"
      (print false nt)
      (CompletedNatWitness.print Terminal.print (minimal' nt))
  );
  iterx (fun nt ->
    fprintf f "maximal(%s) = %s\n"
      (print false nt)
      (NatInfinityMax.print (maximal' nt))
  );
  iterx (fun nt ->
    Printf.fprintf f "follow(%s) = %s\n"
      (print false nt)
      (TerminalSet.print (follow nt))
  );
  ()

(* At log level 3, display the FOLLOW sets for terminal symbols. *)

let () =
  Error.logG 3 @@ fun f ->
  Terminal.iter @@ fun t ->
  fprintf f "follow(%s) = %s\n"
    (Terminal.print t)
    (TerminalSet.print (tfollow t))

(* At log level 3, display the symbolic FOLLOW sets. *)

let () =
  Error.logG 3 @@ fun f ->
  iterx @@ fun nt ->
  fprintf f "sfollow(%s) = %s\n"
    (Nonterminal.print false nt)
    (SymbolSet.print false (sfollow nt))

(* -------------------------------------------------------------------------- *)

let () =
  Time.tick "Analysis of the grammar"
