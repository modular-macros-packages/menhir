(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module helps report errors. *)

open Range

(* ---------------------------------------------------------------------------- *)

(* Logging and log levels. *)

val logG: int -> (out_channel -> unit) -> unit
val logA: int -> (out_channel -> unit) -> unit
val logC: int -> (out_channel -> unit) -> unit

val slogG: int -> ('a, out_channel, unit) format -> 'a
val slogA: int -> ('a, out_channel, unit) format -> 'a
val slogC: int -> ('a, out_channel, unit) format -> 'a

(* ---------------------------------------------------------------------------- *)

(* Errors and warnings. *)

(* [error ps format ...] displays the list of positions [ps], followed with the
   error message [format ...], and exits. The strings "Error: " and "\n" are
   automatically added at the beginning and end of the error message. The
   message should begin with a lowercase letter and end with a dot. *)

val error: ranges -> ('a, out_channel, unit, 'b) format4 -> 'a

(* [warning] is like [error], except it prints "Warning: " instead of
   "Error: " and it does not exit. *)

val warning: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

(* [note] is like [warning], but prints "Note: " instead of "Warning: ". *)

val note: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

(* [log format ...] is like [warning], but prints nothing instead of
   "Warning: ". A newline "\n" is still appended to the message. *)

val log: ('a, out_channel, unit, unit) format4 -> 'a

(* ---------------------------------------------------------------------------- *)

(* Delayed error reports -- where multiple errors can be reported at once. *)

(* A category of errors. *)

type category

(* [new_category()] creates a new category of errors. *)

val new_category: unit -> category

(* [signal category] is like [error], except it does not exit immediately. It
   records the fact that an error of this category has occurred. This can be
   later detected by [exit_if category]. *)

val signal: category -> ranges -> ('a, out_channel, unit, unit) format4 -> 'a

(* [exit_if category] exits with exit code 1 if [signal category] was
   previously called. Together, [signal] and [exit_if] allow reporting
   multiple errors before aborting. *)

val exit_if: category -> unit

(* [with_new_category f] creates a new category [c], executes [f c], then
   invokes [exit_if c], and transmits the result returned by [f]. It works
   also if [f] raises an exception. *)

val with_new_category: (category -> 'b) -> 'b

(* ---------------------------------------------------------------------------- *)

(* In [--strict] mode, warnings about the grammar are treated as errors. Then,
   when [warning] behaves like [signal warnings], which means that it displays
   "Error:" instead of "Warning:", and signals the category [warnings].
   The function call [exit_if warnings] can then be used to stop the program. *)

val warnings: category
