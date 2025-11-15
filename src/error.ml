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

(* A mechanism to turn all display (logging, warnings, errors) on and off. *)

let enabled =
  ref true

let enable () =
  enabled := true

let disable () =
  enabled := false

(* ---------------------------------------------------------------------------- *)

(* The new OCaml type inference protocol means that Menhir is called twice, first
   with [--infer-write-query], then with [--infer-read-reply]. This means that any
   information messages or warnings issued before OCaml type inference takes place
   are duplicated, unless we do something about it. To address this issue, when
   [--infer-read-reply] is set, we disable all output until the point where we
   read the inferred [.mli] file. Then, we enable it again and continue. *)

(* An alternative idea would be to disable all output when [--infer-write-query]
   is set. However, we would then have no output at all if this command fails. *)

let () =
  Settings.(match infer with
  | IMReadReply _ ->
      disable()
  | _ ->
      ()
  )

(* ---------------------------------------------------------------------------- *)

(* Logging and log levels. *)

let selective_log kind verbosity msg =
  if kind >= verbosity && !enabled then
    Printf.fprintf stderr "%t%!" msg

let logG =
  selective_log !Channels.logG

let logA =
  selective_log !Channels.logA

let logC =
  selective_log !Channels.logC

let slog kind level format =
  let format = format ^^ "\n%!" in
  if kind >= level && !enabled then
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
  let kprintf = if !enabled then Printf.kfprintf else Printf.ikfprintf in
  kprintf continuation stderr
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
  if Settings.strict then
    signal warnings pos
  else
    warning pos
