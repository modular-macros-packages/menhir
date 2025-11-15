(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let output =
  Report.create `Normal stderr

let get max_enabled_level level =
  if level <= max_enabled_level then
    (* This verbosity level is enabled. *)
    output
  else
    (* This verbosity level is disabled. *)
    Report.null

let logG, logA, logC =
  ref 0, ref 0, ref 0

let getG level =
  get !logG level

let getA level =
  get !logA level

let getC level =
  get !logC level
