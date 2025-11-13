(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* Make sure that the middle-end runs before the automaton is constructed. *)
include Middle

include LR0Construction.Make(Grammar)

let () =
  Error.logA 1 @@ fun f ->
  Printf.fprintf f "Built an LR(0) automaton with %d states.\n" n
