(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Syntax

module Make (Error : sig

  (**[warning ranges format ...] reports a warning. It does not abort. *)
  val warning: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

  (**[signal ranges format ...] reports an error, but does not abort.
     Therefore, it is possible to report several errors. *)
  val signal: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

end) (X : sig

  (**[ignore_unused_token t] returns [true] if an ignored-token warning
     about the token [t] should be suppressed. *)
  val ignore_unused_token : terminal -> bool

end) : sig

  (**[check g] performs a number of well-formedness checks on the grammar
     [g]. Multiple warnings and errors can be reported via [warning] and
     [signal].

     [check g] does not raise an exception or exit the process when an error
     is reported. It is up to the caller to notice that at least one error
     has been reported and to take appropriate action. *)
  val check : grammar -> unit

end
