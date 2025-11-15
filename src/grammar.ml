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

module  G = GrammarConstruction.Make(Front)()
include G

(* -------------------------------------------------------------------------- *)

(* Print information messages. *)

open Channels

let () =
  Report.log (getG 1)
    "Grammar has %d nonterminal symbols, among which %d start symbols."
    (Nonterminal.n - Production.start) Production.start

let () =
  Report.log (getG 1)
    "Grammar has %d terminal symbols."
    (Terminal.n - 2)

let () =
  Report.log (getG 1)
    "Grammar has %d productions."
    (Production.n - Production.start)

(* -------------------------------------------------------------------------- *)

(* Based on the result of the analyses NONEMPTY and FIRST, emit warnings and
   errors. *)

open Nonterminal
open Analysis

(* If a start symbol generates the empty language or generates the language
   {epsilon}, report an error. In principle, this could be just a warning.
   However, in [Engine], in the function [start], it is convenient to assume
   that neither of these situations can arise. This means that at least one
   token must be read. *)

let check_empty_start_symbol c =
  Nonterminal.user_start_symbols |> List.iter @@ fun nt ->
  if not (nonempty nt) then
    Report.signal c (positions nt)
      "%s generates the empty language." (print false nt);
  if TerminalSet.is_empty (first nt) then
    Report.signal c (positions nt)
      "%s generates the language {epsilon}." (print false nt)

(* If a nonterminal symbol generates the empty language, issue a warning. *)

let check_empty_symbol c =
  iterx @@ fun nt ->
  if not (nonempty nt) then
    Report.warning c (positions nt)
      "%s generates the empty language." (print false nt)

let () =
  monitor @@ fun c ->
  check_empty_start_symbol c;
  check_empty_symbol c

(* -------------------------------------------------------------------------- *)

(* If requested, dump the results of the analyses. *)

let () =
  let c = getG 2 in
  if Report.live c then begin
    iterx (fun nt ->
      Report.log c "nullable(%s) = %b"
        (print false nt)
        (nullable nt)
    );
    iterx (fun nt ->
      Report.log c "first(%s) = %s"
        (print false nt)
        (TerminalSet.print (first nt))
    );
    iterx (fun nt ->
      Report.log c "minimal(%s) = %s"
        (print false nt)
        (CompletedNatWitness.print Terminal.print (minimal' nt))
    );
    iterx (fun nt ->
      let p = maximal nt in
      Report.log c "maximal(%s) = %s"
        (print false nt)
        (if p = max_int then "infinity" else string_of_int p)
    );
    iterx (fun nt ->
      Report.log c "follow(%s) = %s"
        (print false nt)
        (TerminalSet.print (follow nt))
    )
  end

(* At log level 3, display the FOLLOW sets for terminal symbols. *)

let () =
  let c = getG 3 in
  if Report.live c then
    Terminal.iter @@ fun t ->
    Report.log c "follow(%s) = %s"
      (Terminal.print t)
      (TerminalSet.print (tfollow t))

(* At log level 3, display the symbolic FOLLOW sets. *)

let () =
  let c = getG 3 in
  if Report.live c then
    iterx @@ fun nt ->
    Report.log c "sfollow(%s) = %s"
      (Nonterminal.print false nt)
      (SymbolSet.print false (sfollow nt))
