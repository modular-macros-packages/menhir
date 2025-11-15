(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Channels
open Grammar

(* -------------------------------------------------------------------------- *)

(* If requested, print the reference graph of the grammar. *)

let () =
  if Settings.reference_graph then
    let module R = ReferenceGraph.Make(Grammar) in
    R.print (Settings.base ^ ".dot")

(* -------------------------------------------------------------------------- *)

(* If [--random-sentence] was specified on the command line, obey it. *)

let () =
  Settings.random_sentence |> Option.iter begin fun (nt, goal, style) ->
    match Nonterminal.lookup nt with
    | exception Not_found ->
        Report.Just.error [] "the nonterminal symbol %s does not exist." nt
    | nt ->
        let module R = RandomSentenceGenerator.Make(Grammar) in
        let sentence = R.nonterminal nt goal in
        print_endline (Sentence.print style (Some nt, sentence));
        exit 0
  end

(* ------------------------------------------------------------------------ *)

(* Perform loop detection before attempting to build the LR(0) automaton. *)

let () =
  monitor @@ fun c ->
  Time.time "Detecting loops" @@ fun () ->
  let module L = LoopDetection.Make(Grammar) in
  L.detect_cycle c;
  L.detect_hidden_left_recursion c
