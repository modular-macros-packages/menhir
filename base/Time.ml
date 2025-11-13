(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let c =
  Report.create()

let[@inline] now () =
  let open Unix in
  (times()).tms_utime

let time msg (task : unit -> 'a) : 'a =
  let times1 = now() in
  let result = task() in
  let times2 = now() in
  let elapsed = times2 -. times1 in
  Report.log c "%s: %.02fs" msg elapsed;
  result

let output_channel =
  ref None

let set_output_channel ochannel =
  output_channel := ochannel

let () =
  at_exit @@ fun () ->
  !output_channel |> Option.iter @@ fun f ->
  Report.flush f c;
  close_out f

type start =
  float

let start =
  now

let[@inline] stop times1 msg =
  let times2 = now() in
  let elapsed = times2 -. times1 in
  Report.log c "%s: %.02fs" msg elapsed
