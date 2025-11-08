(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf
open Grammar

module R =
  ReferenceInterpreter.Make(Lr1)

module StackSymbolsShort =
  StackSymbols.Short(Lr1)()

(* --------------------------------------------------------------------------- *)

(* Type definitions. *)

type sentence =
  Sentence.sentence

type spurious_reduction =
  R.spurious_reduction

type target =
  R.target

(* --------------------------------------------------------------------------- *)

(* [interpret_sentence] interprets a sentence and produces just one line of
   output. Thus, in case multiple sentences are supplied, it should be clear
   to the user which outcomes correspond to which sentences. *)

let interpret_sentence show_cst sentence : unit =
  let nt = Sentence.start sentence
  and _, toks = sentence in
  let module T = TerminalStream.Make(Grammar) in

  (* Run the reference interpreter. This can produce a concrete syntax tree
     ([Some cst]), fail with a parser error ([None]), or fail with a lexer
     error ([EndOfStream]). *)

  let lexer, lexbuf = T.stream toks in
  let strategy = Settings.strategy
  and show = Settings.trace
  and count = ref 0 (* unused *) in
  begin match
    R.interpret strategy show count nt lexer lexbuf
  with

  | Some cst ->
      (* Success. *)
      printf "ACCEPT";
      begin match show_cst with
      | `ShowCST ->
          print_newline();
          CST.show stdout cst
      | `DoNotShowCST ->
          ()
      end

  | None ->
      (* Parser failure. *)
      printf "REJECT"

  | exception T.EndOfStream ->
      (* Lexer failure. *)
      printf "OVERSHOOT"

  end;
  print_newline()

(* --------------------------------------------------------------------------- *)

(* [interpret_error_sentence] interprets a sentence, expecting it to end
   in an error. Failure or success is reported via two continuations. *)

let interpret_error_sentence show sentence fail succeed =
  let nt = Sentence.start sentence
  and (_, terminals) = sentence in
  let strategy = Settings.strategy
  and count = ref 0 (* unused *) in
  match R.check_error_path strategy show count nt terminals with
  | R.OInputReadPastEnd ->
      fail "no syntax error occurs."
  | R.OInputNotFullyConsumed ->
      fail "a syntax error occurs before the last token is reached."
  | R.OUnexpectedAccept ->
      fail "no syntax error occurs; in fact, this input is accepted."
  | R.OK target ->
      succeed target

(* --------------------------------------------------------------------------- *)

(* This default error message is produced by [--list-errors] when it creates a
   [.messages] file, and is recognized by [--compare-errors] when it compares
   two such files. *)

let default_message =
  "<YOUR SYNTAX ERROR MESSAGE HERE>\n"

(* --------------------------------------------------------------------------- *)

(* [print_messages_auto (sentence, target)] displays the sentence [sentence],
   leading to the state [target]. It then displays a bunch of auto-generated
   comments. *)

let print_messages_auto (sentence, target) =

  (* Print the sentence. *)
  print_string (Sentence.print `Abstract sentence);

  (* If a token alias has been defined for every terminal symbol, then
     we can convert this sentence into concrete syntax. Do so. We make
     a few assumptions about the concrete syntax of the language:
       1. It is permitted to insert one space between two tokens;
       2. No token contains a newline character.
          (Our lexer enforces this assumption.)
     The name of the start symbol cannot be printed in a meaningful
     manner, so it is omitted. *)
  if Terminal.every_token_has_an_alias then
    printf
      "##\n\
       ## Concrete syntax: %s\n"
      (Sentence.print `Concrete sentence)
  ;

  (* Show which state this sentence leads to. *)
  let (s', spurious) = target in
  printf
    "##\n\
     ## Ends in an error in state: %d.\n\
     ##\n\
     %s##\n"
    (Lr1.encode s')
    (* [Lr0.ALR1.print] prints just the state's kernel, not its closure.
       Displaying the closure could be helpful, but is usually intolerably
       verbose. *)
    (Lr0.ALR1.print "## " (Lr1.state s'))
  ;

  (* Show the known suffix of the stack in this state. *)
  printf
    "## The known suffix of the stack is as follows:\n\
     ##%s\n\
     ##\n"
    (Symbol.print_array' false (StackSymbolsShort.node_shape s'))
  ;

  (* If interpreting this sentence causes spurious reductions (that is,
     reductions that take place after the last terminal symbol has been
     shifted), say so, and show them. *)
  if spurious <> [] then begin
    printf
      "## WARNING: This example involves spurious reductions.\n\
       ## This implies that, although the LR(1) items shown above provide an\n\
       ## accurate view of the past (what has been recognized so far), they\n\
       ## may provide an INCOMPLETE view of the future (what was expected next).\n"
    ;
    List.iter (fun (s, prod) ->
      printf
        "## In state %d, spurious reduction of production %s\n"
        (Lr1.encode s)
        (Production.print prod)
    ) spurious;
    printf "##\n"
  end

(* --------------------------------------------------------------------------- *)

(* [print_messages_item] displays one entry in the [.messages] file format. *)

let print_messages_item (sentence, target) =
  (* Print the sentence, followed with auto-generated comments. *)
  print_messages_auto (sentence, target);
  (* Then, print a proposed error message, between two blank lines. *)
  printf "\n%s\n" default_message

(* --------------------------------------------------------------------------- *)

(* [interpret_error] interprets a sentence, expecting it to end in an error.
   Failure or success is reported on the standard output channel. *)

let interpret_error sentence =
  Report.monitor `Strict @@ fun c ->
  let show = Settings.trace
  and fail msg =
    Report.error c [] "%s" msg
  and succeed target =
    print_messages_item (sentence, target);
    exit 0
  in
  interpret_error_sentence show sentence fail succeed

(* --------------------------------------------------------------------------- *)

(* [setup c] returns a function [read] which reads one raw sentence from the
   standard input channel and immediately validates it, producing a sentence. *)

let setup c : unit -> sentence option =

  let open Lexing in
  let lexbuf = from_channel stdin in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "(stdin)" };

  let read () =
    match RawSentenceParser.optional_sentence RawSentenceLexer.lex lexbuf with
    | exception RawSentenceLexer.Error ->
        Report.error c [Range.current lexbuf]
          "unexpected character.\n\
           (I believe I am reading a sentence, but may be off.)"
    | exception Parsing.Parse_error ->
        Report.error c [Range.current lexbuf]
          "ill-formed input sentence."
    | osentence ->
        Validate.option Sentence.validate_sentence c osentence
  in

  read

(* --------------------------------------------------------------------------- *)

(* The two commands. *)

module Command = struct

  let interpret show_cst =
    Report.monitor `Strict @@ fun c ->
    let read = setup c in
    printf "Ready!\n%!";
    while true do
      match read() with
      | None ->
          exit 0
      | Some sentence ->
          interpret_sentence show_cst sentence
    done

  let interpret_error () =
    Report.monitor `Strict @@ fun c ->
    let read = setup c in
    match read() with
    | None ->
        exit 1 (* abnormal: no input *)
    | Some sentence ->
        interpret_error sentence (* never returns *)

end (* Command *)
