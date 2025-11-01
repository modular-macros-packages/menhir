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
    AutomatonGraph.print_automaton_graph()

(* Let [Interpret] handle the command line options [--interpret],
   [--interpret-error], [--compile-errors], [--compare-errors]. *)

let () =
  Interpret.run()

(* If [--list-errors] is set, produce a list of erroneous input sentences,
   then stop. *)

let () =
  if Settings.list_errors then begin
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
  end

(* If requested, generate a .cmly file. *)

let () =
  if Settings.cmly then
    Cmly_write.write (Settings.base ^ ".cmly")

(* If no code generation was requested (i.e., no back-end was selected)
   then exit at this point. *)

let () =
  if Settings.backend = `NoBackend then
    exit 0
