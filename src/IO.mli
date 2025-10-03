(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers input-output utilities. *)

(**[moving_away filename action] moves the file [filename] away (if it
   exists), performs [action], then moves the file back into place (if
   it was moved away). *)
val moving_away: string -> (unit -> 'a) -> 'a

(**[with_file filename creation action] creates the file [filename] by
   running [creation], then runs [action], and ensures that the file
   is removed in the end. *)
val with_file: string -> (unit -> unit) -> (unit -> 'a) -> 'a

(**[invoke command] invokes an external command (which expects no input).
   If the command succeeds, its output is returned. Otherwise, [None] is
   returned. *)
val invoke: string -> string option

(**[read_whole_file filename] reads the file [filename] in text mode and
   returns its contents as a string. *)
val read_whole_file: string -> string
