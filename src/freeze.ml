(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let () =
  if Settings.automaton_graph then
    let module A = AutomatonGraph.Make(Lr1) in
    A.print (Settings.base ^ ".dot")

(* Handle the command line options [--interpret], [--interpret-error],
   [--compile-errors], [--compare-errors], etc. *)

let () =
  match Settings.interpret with
  | `No ->
      ()
  | `Normal show_cst ->
      Interpret.Command.interpret show_cst
  | `Error ->
      Interpret.Command.interpret_error()

let () =
  Settings.compile_errors |> Option.iter @@ fun filename ->
  Interpret.Command.compile_errors filename;
  exit 0

let () =
  Settings.compare_errors |> Option.iter @@ fun (filename1, filename2) ->
  Interpret.Command.compare_errors filename1 filename2;
  exit 0

let () =
  Settings.merge_errors |> Option.iter @@ fun (filename1, filename2) ->
  Interpret.Command.merge_errors filename1 filename2;
  exit 0

let () =
  Settings.update_errors |> Option.iter @@ fun filename ->
  Interpret.Command.update_errors filename;
  exit 0

let () =
  Settings.echo_errors |> Option.iter @@ fun filename ->
  Interpret.Command.echo_errors filename;
  exit 0

let () =
  Settings.echo_errors_concrete |> Option.iter @@ fun filename ->
  Interpret.Command.echo_errors_concrete filename;
  exit 0

(* If [--list-errors] is set, produce a list of erroneous input sentences,
   then stop. *)

let () =
  if Settings.list_errors then
    let module X = struct
      (* Undocumented: if [--log-automaton 2] is set, be verbose. *)
      let verbose = Settings.logA >= 2
      (* For my own purposes, LRijkstra can print one line of statistics to a .csv file. *)
      let statistics = if false then Some "lr.csv" else None
    end in
    let module L = struct
      include LRijkstra.Run(X)()
    end in
    exit 0

(* If requested, generate a .cmly file. *)

let () =
  if Settings.cmly then
    Cmly_write.write (Settings.base ^ ".cmly")

(* If no code generation was requested (i.e., no back-end was selected)
   then exit at this point. *)

let () =
  if Settings.backend = `NoBackend then
    exit 0
