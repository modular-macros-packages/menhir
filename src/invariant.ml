(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This artificial dependency ensures that Freeze runs before we execute
   the costly analyses that follow. *)

module F = Freeze

module ErrorC1 = struct
  let log format = Error.slogC 1 format
end

include StackShape.Make(Lr1)(Settings)(ErrorC1)

let () =
  Error.logC 3 @@ fun f ->
  Short.print_stack_states f

let () =
  Error.logC 3 @@ fun f ->
  print_represented f

let () =
  Error.logC 3 @@ fun f ->
  Long.print_stack_states f
