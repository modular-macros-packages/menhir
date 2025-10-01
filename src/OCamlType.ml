(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* An OCaml type is either a thing (if it was found in some
   source file) or a string (if it was inferred via [Infer]). *)

open Located

type ocamltype =
  | Declared of string located
  | Inferred of string
