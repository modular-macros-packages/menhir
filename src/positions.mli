(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Range

(** Decoration of a value with a position. *)
type 'a located =
    {
      value    : 'a;
      position : range;
    }

(** [value dv] returns the raw value that underlies the
    decorated value [dv]. *)
val value: 'a located -> 'a

(** [position dv] returns the position that decorates the
    decorated value [dv]. *)
val position: 'a located -> range

(** [decompose dv] returns a pair of the value and position. *)
val decompose: 'a located -> 'a * range

(** [with_pos p v] decorates [v] with a position [p]. *)
val with_pos : range -> 'a -> 'a located
val with_loc : (Lexing.position * Lexing.position) -> 'a -> 'a located

val unknown_pos : 'a -> 'a located

(** [map f v] extends the decoration from [v] to [f v]. *)
val map: ('a -> 'b) -> 'a located -> 'b located
val pmap: (range -> 'a -> 'b) -> 'a located -> 'b located

val fragment : 'a located -> 'a Fragment.fragment

(* Low-level printing function, for debugging. *)
val print: Lexing.position -> unit
