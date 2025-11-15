(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let strict =
  ref false

let mode () =
  if !strict then `WarningIsSignal else `Normal

let monitor action =
  Report.monitor (mode()) action

let info =
  Report.create `Normal stderr

let get max_enabled_level level =
  if level <= max_enabled_level then
    (* This verbosity level is enabled. *)
    info
  else
    (* This verbosity level is disabled. *)
    Report.null

let logG, logA, logC =
  ref 0, ref 0, ref 0

let[@inline] getG level =
  get !logG level

let[@inline] getA level =
  get !logA level

let[@inline] getC level =
  get !logC level
