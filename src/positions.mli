(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(** Abstract type for pairs of positions in the lexing stream. *)
type t

(** Decoration of a value with a position. *)
type 'a located =
    {
      value    : 'a;
      position : t;
    }

(** [value dv] returns the raw value that underlies the
    decorated value [dv]. *)
val value: 'a located -> 'a

(** [position dv] returns the position that decorates the
    decorated value [dv]. *)
val position: 'a located -> t

(** [decompose dv] returns a pair of the value and position. *)
val decompose: 'a located -> 'a * t

(** [with_pos p v] decorates [v] with a position [p]. *)
val with_pos : t -> 'a -> 'a located
val with_loc : (Lexing.position * Lexing.position) -> 'a -> 'a located

val unknown_pos : 'a -> 'a located

(** [map f v] extends the decoration from [v] to [f v]. *)
val map: ('a -> 'b) -> 'a located -> 'b located
val pmap: (t -> 'a -> 'b) -> 'a located -> 'b located

(** This value is used when an object does not come from
    a particular input location. *)
val dummy: t

val start_of_position: t -> Lexing.position

val end_of_position: t -> Lexing.position

val fragment : 'a located -> 'a Fragment.fragment

val import : Lexing.position * Lexing.position -> t

(** [string_of_pos p] returns the standard (Emacs-like) representation
    of the position [p]. *)
val string_of_pos : t -> string

(** [cpos lexbuf] returns the current position of the lexer. *)
val cpos : Lexing.lexbuf -> t

(* The functions that print error messages and warnings require a list of
   positions. The following auxiliary functions help build such lists. *)

type positions =
    t list

val one: Lexing.position -> positions

val lexbuf: Lexing.lexbuf -> positions

(* Low-level printing function, for debugging. *)
val print: Lexing.position -> unit
