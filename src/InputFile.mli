(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module defines a type of input files, which is used to record the
   origin of certain elements (productions, declarations, etc.). It has
   global state: it keeps track of which input file is currently being read. *)

(**An input file. *)
type file

(* [get_input_file()] indicates which input file is currently being read. *)

val get_input_file: unit -> file

(**[get_file_name] extracts a file name out of an input file. *)
val get_file_name : file -> string

(* This fictitious "built-in" input file is used as the origin of the start
   productions. This technical detail is probably irrelevant entirely. *)

val builtin_input_file: file

(* This equality test for input files is used (for instance) when determining
   which of two productions has greater priority. *)

val same_input_file: file -> file -> bool

(* This ordering between input files reflects their ordering on the command
   line. Ideally, it should NOT be used. *)

val compare_input_files: file -> file -> int

(* -------------------------------------------------------------------------- *)

(* The contents of the current input file. *)

(**[with_file_content filename contents f] records that the current input
   file has name [filename] and content [content] while the action [f] runs.
   The function [f] can then call {!chunk} to retrieve certain segments of
   [content]. *)
val with_file_content: string -> string -> (unit -> 'a) -> 'a

(* [chunk pos1 pos2] extracts a chunk out of the current input file, delimited
   by the positions [pos1] and [pos2]. *)

val chunk: (Lexing.position * Lexing.position) -> string
