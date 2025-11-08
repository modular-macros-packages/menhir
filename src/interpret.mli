(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Grammar

(**A target indicates which state a sentence leads to
   and which spurious reductions are performed at the end.
   See [ReferenceInterpreter]. *)
type target =
  Lr1.node * spurious_reduction list

and spurious_reduction =
  Lr1.node * Production.t

(**This default error message is produced by [--list-errors] when it creates a
   [.messages] file, and is recognized by [--compare-errors] when it compares
   two such files. *)
val default_message: string

(**[print_messages_auto (sentence, target)] prints part of an item in the
   [.messages] file format. It prints the sentence [sentence], followed with
   auto-generated comments, indicating that this sentence leads to an error in
   the state [target], with spurious reductions also described by [target].
   The error message that is desired in this situation is {i not} printed.  *)
val print_messages_auto: Sentence.sentence * target -> unit

(**[print_messages_item (sentence, target)] prints an item in the [.messages]
   file format. The first part of this item is printed by [print_messages_auto].
   The second part is the default message {!default_message} between two blank
   lines. *)
val print_messages_item: Sentence.sentence * target -> unit

(**[interpret_error_sentence show sentence fail succeed] interprets the sentence
   [sentence], expecting it to end in an error. The parameter [trace] determines
   whether log messages are emitted; it corresponds to [--trace]. Failure or
   success is reported via the continuations [fail] and [succeed]. The
   continuation [fail] is invoked if the expected error is {i not} encountered;
   its an argument is an error message. The continuation [succeed] is invoked if
   the expected error is encountered: its argument is the target that has been
   reached. *)
val interpret_error_sentence:
  (* trace:    *) bool ->
  (* sentence: *) Sentence.sentence ->
  (* failure:  *) (string -> 'a) ->
  (* success:  *) (target -> 'a) ->
                  'a

module Command : sig

  (**[interpret show_cst] is a potentially endless interactive loop. It reads
     sentences from the standard input channel and interprets them. If an empty
     sentence is read then the process is stopped with exit code 0. To allow
     interactive use, each sentence is interpreted as soon as it is read. This
     behavior corresponds to [--interpret] and [--interpret-show-cst]. *)
  val interpret: [`ShowCST | `DoNotShowCST] -> unit

  (**[interpret_error()] reads one sentence from the standard input channel and
     interprets it. It confirms that this sentence ends in an error and displays
     the number of the state that is reached. Then the process is stopped with
     exit code 0. This behavior corresponds to [--interpret-error]. *)
  val interpret_error: unit -> unit

end
