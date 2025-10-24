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

(* -------------------------------------------------------------------------- *)

(* If [--random-sentence] was specified on the command line, obey it. *)

let () =
  Settings.random_sentence |> Option.iter begin fun (nt, goal, style) ->
    match Nonterminal.lookup nt with
    | exception Not_found ->
        Error.error [] "the nonterminal symbol %s does not exist." nt
    | nt ->
        let module R = RandomSentenceGenerator.Make(Grammar) in
        let sentence = R.nonterminal nt goal in
        print_endline (Sentence.print style (Some nt, sentence));
        exit 0
  end

(* ------------------------------------------------------------------------ *)

(* Perform loop detection before attempting to build the LR(0) automaton. *)

let () =
  let module L = LoopDetection.Run(Error)(Grammar) in
  ()

let () =
  Time.tick "Running loop detection"
