(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers facilities for delaying and displaying error messages.  *)

open Range

(**The functions offered by this module are organized in several groups. The
   simpler functions, such as {!error'} and {!warning'}, do not take a channel
   as an argument; they have an immediate effect. The more complex functions,
   such as {!signal} and {!warning}, take a channel as an argument; they have
   a delayed effect, since the error message is delayed (buffered) by the
   channel. To obtain a fresh channel and use it in a correct way, the
   recommended way is to use {!monitor}. The functions {!create} and
   {!exit_if} offer an alternative way, which is slightly more powerful, but
   more complex. *)

(** {2 Functions that Do Not Need a Channel} *)

(**[error' ranges format ...] is a short-hand for
   [let c = create() in error c ranges format ...].
   The error message is displayed immediately
   and the process is aborted with exit status 1.
   [error'] is preferable to [error] in situations
   where one wishes to immediately display a single message and abort. *)
val error': ranges -> ('a, Buffer.t, unit, 'b) format4 -> 'a

(**[warning' ranges format ...] is a short-hand for
   [let c = create() in warning c ranges format ...; flush stderr c].
   The warning is displayed immediately.
   [warning'] is preferable to [warning] in situations
   where one wishes to immediately display a single warning message. *)
val warning': ranges -> ('a, Buffer.t, unit, unit) format4 -> 'a

(** {2 Functions that Need a Channel} *)

(**A channel is a destination to which messages can be written. All output is
   held in a buffer in memory. An explicit call to [flush] is necessary for
   the output to become visible. *)
type channel

(**[signal c ranges format ...] writes an error message to the channel [c].
   The message begins with the list of ranges [ranges]. The remainder of the
   message is determined by the format [("Error: " ^^ format ^^ "\n") ...].
   Thus, the string produced by [format ...] should begin with a lowercase
   letter and end with a dot. The channel is not flushed. *)
val signal: channel -> ranges -> ('a, Buffer.t, unit, unit) format4 -> 'a

(**[error c ranges format ...] first invokes [signal c ranges format ...],
   then flushes [c] to [stderr] and aborts the process with exit status 1. *)
val error: channel -> ranges -> ('a, Buffer.t, unit, 'b) format4 -> 'a

(**[warning] is like [signal], but says ["Warning: "] instead of ["Error: "]. *)
val warning: channel -> ranges -> ('a, Buffer.t, unit, unit) format4 -> 'a

(**[note] is like [signal], but says ["Note: "] instead of ["Error: "]. *)
val note: channel -> ranges -> ('a, Buffer.t, unit, unit) format4 -> 'a

(**[log] is like [signal], but says nothing instead of ["Error: "].
   A newline "\n" is still appended to the message. *)
val log: channel -> ('a, Buffer.t, unit, unit) format4 -> 'a

(**[flush f c] emits the content of the channel [c] on the output channel [f].
   The channel [c] is emptied, so two successive calls to [flush f c] have the
   same effect as just one call. The number of messages that have been written
   to the channel [c] is not reset, so that calling [flush] in the scope of
   [monitor] is permitted and does not disturb [monitor]. *)
val flush: out_channel -> channel -> unit

(** {2 Functions that Create Channels} *)

(**[monitor strict action] applies the function [action] to a fresh channel [c].
   Once [action] terminates (either normally or by raising an exception), the
   channel is flushed to [stderr]. Furthermore, if [strict] is [`Strict] and if
   the number of messages that have been written to the channel [c] is nonzero
   then the process is aborted with exit status 1. *)
val monitor : [`Strict | `Lax] -> (channel -> 'a) -> 'a

(**[create()] creates a fresh channel. *)
val create: unit -> channel

(**[exit_if c] flushes the channel [c] to [stderr]. Furthermore, if the number
   of messages that have been written to the channel [c] is nonzero then the
   process is aborted with exit status 1. *)
val exit_if: channel -> unit
