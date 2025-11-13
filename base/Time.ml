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

let b =
  Buffer.create 1024

let[@inline] now () =
  let open Unix in
  (times()).tms_utime

let time msg (task : unit -> 'a) : 'a =
  let times1 = now() in
  let result = task() in
  let times2 = now() in
  let elapsed = times2 -. times1 in
  bprintf b "%s: %.02fs\n" msg elapsed;
  result

let output_channel =
  ref None

let set_output_channel ochannel =
  output_channel := ochannel

let () =
  at_exit @@ fun () ->
  !output_channel |> Option.iter @@ fun f ->
  fprintf f "%s" (Buffer.contents b);
  close_out f

type start =
  float

let start =
  now

let[@inline] stop times1 msg =
  let times2 = now() in
  let elapsed = times2 -. times1 in
  bprintf b "%s: %.02fs\n" msg elapsed
