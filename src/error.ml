(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf

(* ---------------------------------------------------------------------------- *)

(* Logging and log levels. *)

let slog kind level format =
  let format = format ^^ "\n%!" in
  if kind >= level then
    Printf.eprintf format
  else
    Printf.ifprintf stderr format

let slogG level format =
  slog !Channels.logG level format

let slogC level format =
  slog !Channels.logC level format

let slogA level format =
  slog !Channels.logA level format

(* ---------------------------------------------------------------------------- *)

(* Errors and warnings. *)

let print_positions f positions =
  List.iter (fun position ->
    fprintf f "%s:\n" (Range.show position)
  ) positions

let display continuation header positions format =
  Printf.kfprintf continuation stderr
    ("%a" ^^ header ^^ format ^^ "\n%!")
    print_positions positions

let error positions format =
  display
    (fun _ -> exit 1)
    "Error: "
    positions format

let warning positions format =
  display
    (fun _ -> ())
    "Warning: "
    positions format

let note positions format =
  display
    (fun _ -> ())
    "Note: "
    positions format

let log format =
  display
    (fun _ -> ())
    ""
    [] format

(* ---------------------------------------------------------------------------- *)

(* Delayed error reports -- where multiple errors can be reported at once. *)

type category =
  bool ref

let new_category () =
  ref false

let signal category positions format =
  display
    (fun _ -> category := true)
    "Error: "
    positions format

let exit_if category =
  if !category then
    exit 1

let with_new_category f =
  let c = new_category() in
  match f c with
  | y ->
      exit_if c;
      y
  | exception e ->
      exit_if c;
      raise e

(* ---------------------------------------------------------------------------- *)

(* Warnings can optionally be treated as errors. *)

let warnings =
  new_category()

let warning pos =
  if !Channels.strict then
    signal warnings pos
  else
    warning pos
