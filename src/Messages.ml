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
open OrComment
open RawSentence
open Validate
open Grammar

let default_message = Interpret.default_message
let print_messages_auto = Interpret.print_messages_auto
let interpret_error_sentence = Interpret.interpret_error_sentence

(* --------------------------------------------------------------------------- *)

(* Type definitions. *)

type sentence =
  Sentence.sentence

(* A located sentence is a sentence
   together with its start and end positions. *)

type located_sentence =
  Range.ranges * sentence

let ranges ((ranges, _) : located_sentence) =
  ranges

(* A target tells us which state a sentence leads to, as well
   as which spurious reductions are performed at the end. *)

type target =
  Lr1.node * spurious_reduction list

and spurious_reduction =
  Lr1.node * Production.t

let target2state (s, _spurious) =
  s

(* A targeted sentence is a located sentence
   together with the target to which it leads.  *)

type targeted_sentence =
  located_sentence * target

(* A delimiter. *)

type delimiter =
  string

(* An error message. *)

type message =
  string

(* A run is a series of targeted sentences or comments, followed with a
   delimiter (composed at least one blank line and possibly comments),
   followed with an error message. *)

type run = {
  (* A list of sentences. *)
  elements: targeted_sentence or_comment list;
  (* A delimiter. *)
  delimiter: delimiter;
  (* A message. *)
  message: message;
}

(* The name of a [.messages] file. *)
type filename =
  string

(* --------------------------------------------------------------------------- *)

(* [write_run run] writes a run into a new [.messages] file. Manually-written
   comments are preserved. New auto-generated comments are produced. *)

let write_run : run or_comment -> unit =
  function
  | Thing run ->
      (* First, print every sentence and human comment. *)
      List.iter (fun sentence_or_comment ->
        match sentence_or_comment with
        | Thing ((_ranges, sentence), target) ->
            (* Every sentence is followed with newly generated auto-comments. *)
            print_messages_auto (sentence, target)
        | Comment c ->
            print_string c
      ) run.elements;
      (* Then, print the delimiter, which must begin with a blank line
         and may include comments. *)
      print_string run.delimiter;
      (* Then, print the error message. *)
      print_string run.message
      (* No need for another blank line. It will be printed as part of a
         separate [Comment]. *)
  | Comment comments ->
      (* Must begin with a blank line. *)
      print_string comments

(* --------------------------------------------------------------------------- *)

(* The lexer [RawSentenceLexer] produces sentences that contain raw symbols,
   that is, strings that are not yet known to represent valid nonterminal
   or terminal symbols. This check is performed here. It either succeeds
   or signals an error in the category [c] and raises [Invalid]. *)

(* We also check that every sentence leads to an error state. *)

let validate_sentence
: (raw_sentence, targeted_sentence) validator =
fun c raw_sentence ->
  (* First, validate every symbol. *)
  let sentence = Sentence.validate_sentence c raw_sentence in
  (* Then, check that this sentence leads to an error state. *)
  let ranges = [RawSentence.range raw_sentence] in
  let show = false
  and fail msg =
    Report.signal c ranges
      "this sentence does not end with a syntax error, as it should:\n%s" msg;
    raise Invalid
  and succeed target =
    (ranges, sentence), target
  in
  interpret_error_sentence show sentence fail succeed

(* [validate_entry] validates a list of sentences or comments, as returned by
   [RawSentenceParser.entry]. If a sentence contains an error, then an error
   message is emitted, this sentence is removed from the list, and the
   validation process continues. *)

let validate_entry : (raw_entry, targeted_sentence or_comment list) validator =
  Validate.robust_list (OrComment.validate validate_sentence)

(* --------------------------------------------------------------------------- *)

(* Display an informational message about the contents of a [.messages] file.  *)

let stats (runs : run or_comment list) =
  (* [s] counts the sample input sentences. [m] counts the error messages. *)
  let s = ref 0
  and m = ref 0 in
  List.iter (function
  | Thing { elements; _ } ->
      incr m;
      List.iter (function
      | Thing _ ->
          incr s
      | Comment _ ->
          ()
      ) elements
  | Comment _ ->
      ()
  ) runs;
  eprintf
    "Read %d sample input sentences and %d error messages.\n%!"
    !s !m

(* --------------------------------------------------------------------------- *)

(* Reading a [.messages] file. *)

(* Our life is slightly complicated by the fact that the whitespace between
   two runs can contain comments, which we wish to preserve when performing
   [--update-errors]. *)

(* Sentences that do not pass validation are removed (and error messages are
   emitted). If one or more validation errors have occurred and if [strict]
   is [true], then we stop at the end. *)

let mkcomment c accu =
  if String.length c = 0 then accu else Comment c :: accu

let read_messages strict filename : run or_comment list =
  let open Segment in
  Report.monitor strict @@ fun c ->
  (* Read and segment the file. *)
  let segments : (tag * string * Lexing.lexbuf) list = segment filename in
  (* Process the segments, two by two. We expect one segment to contain
     a non-empty series of sentences, and the next segment to contain
     free-form text. *)
  let rec loop accu segments =
    match segments with
    | [] ->
        List.rev accu
    | (Whitespace, comments, _) :: segments ->
        loop (mkcomment comments accu) segments
    | (Segment, _, lexbuf) :: segments ->
        (* Read a series of raw sentences. *)
        match RawSentenceParser.entry RawSentenceLexer.lex lexbuf with
        | exception Parsing.Parse_error ->
            Report.error c [Range.current lexbuf]
              "ill-formed sentence."
        | elements ->
            (* [elements] is a list of raw sentences or comments. Validate it.
               Any sentences that do not pass validation are removed (and
               error messages are emitted). In an effort to be robust, we
               continue. If there remain zero sentences, then this entry is
               removed entirely. *)
            let elements = validate_entry c elements in
            (* In principle, we should now find a segment of whitespace
               followed with a segment of text. By construction, the two
               kinds of segments alternate. *)
            match segments with
            | (Whitespace, delimiter, _) ::
              (Segment, message, _) ::
              segments ->
                if OrComment.count elements = 0 then
                  (* There remain zero sentences. Skip this entry. *)
                  loop accu segments
                else
                  (* Accumulate this entry. *)
                  let run = { elements; delimiter; message } in
                  loop (Thing run :: accu) segments
            | []
            | [ _ ] ->
                Report.error c [Range.current lexbuf]
                  "missing a final message. I may be desynchronized."
            | (Segment, _, _) :: _
            | (Whitespace, _, _) :: (Whitespace, _, _) :: _ ->
                (* Should not happen, thanks to the alternation between the
                   two kinds of segments. *)
                assert false
  in
  let runs = loop [] segments in
  Report.flush stderr c; (* optional: show error messages before statistics *)
  stats runs;
  runs

(* --------------------------------------------------------------------------- *)

(* [foreach_targeted_sentence runs] enumerates the targeted sentences and
   corresponding messages in the list [runs]. *)

let foreach_targeted_sentence (runs : run or_comment list) yield =
  runs |> List.iter @@ fun run ->
  run |> OrComment.iter @@ fun run ->
  run.elements |> List.iter @@ fun sentence ->
  sentence |> OrComment.iter @@ fun sentence ->
  yield sentence run.message

(* --------------------------------------------------------------------------- *)

(* [message_table] converts a list of targeted runs to a table (a mapping) of
   states to located sentences and messages. Optionally, it can detect that
   two sentences lead to the same state, and report an error. *)

let message_table
    (detect_redundancy : bool)
    (runs : run or_comment list)
  : (located_sentence * message) Lr1.NodeMap.t =

  Report.monitor `Strict @@ fun c ->
  let table = ref Lr1.NodeMap.empty in
  let () =
    foreach_targeted_sentence runs @@ fun (sentence2, target) message ->
    let s = target2state target in
    match Lr1.NodeMap.find s !table with
    | sentence1, _ ->
        if detect_redundancy then
          Report.signal c (ranges sentence1 @ ranges sentence2)
            "these sentences both cause an error in state %d."
            (Lr1.encode s)
    | exception Not_found ->
        table := Lr1.NodeMap.add s (sentence2, message) !table
  in
  !table

(* --------------------------------------------------------------------------- *)

(* [compile_runs] converts a list of targeted runs to OCaml code that encodes
   a mapping of state numbers to error messages. The code is sent to the
   standard output channel. *)

let compile_runs filename (runs : run or_comment list) : unit =

  (* We wish to produce a function that maps a state number to a message.
     By convention, we call this function [message]. *)

  let name = "message" in

  let open IL in
  let open ILConstruction in
  let default = {
    branchpat  = PWildcard;
    branchbody = eraisenotfound
  (* The default branch raises an exception, which can be caught by
     the user, who can then produce a generic error message. *)
  } in
  let branches =
    List.fold_left (OrComment.fold (fun branches run ->
      (* Create an or-pattern for these states. *)
      let states =
        run.elements |> MList.filter_map @@ fun sentence_or_element ->
        sentence_or_element
        |> OrComment.map (fun (_, target) ->
            let s = target2state target in
            pint (Lr1.encode s)
          )
        |> OrComment.to_option
      in
      (* Map all these states to this message. *)
      { branchpat = POr states;
        branchbody = EStringConst run.message } :: branches
    )) [ default ] runs
  in
  let messagedef = {
    valpat = PVar name;
    valval = EFun ([ PVar "s" ], EMatch (EVar "s", branches))
  } in
  let program = [
    SIComment (sprintf
      "This file was auto-generated based on \"%s\"." filename);
    SIComment (sprintf
      "Please note that the function [%s] can raise [Not_found]." name);
    SIValDefs (false,
      [ messagedef ]);
  ] in

  (* Write this program to the standard output channel. *)

  let module P = ILPrinter.Make (struct
    let f = stdout
    let print_line_directives = None
    let print_comments = Settings.comment
  end) in
  P.program program

(* --------------------------------------------------------------------------- *)

(* If [--compile-errors <filename>] is set, compile the error message
   descriptions found in file [filename] down to OCaml code, then stop. *)

let compile_errors filename =
  (* Read the file. Compute the target state of every sentence. Stop if a
     sentence does not end in an error state, as expected. *)
  let runs : run or_comment list = read_messages `Strict filename in

  (* Build a mapping of states to located sentences. This allows us to
     detect if two sentences lead to the same state. *)
  let _ = message_table true runs in

  (* In principle, we would like to check whether this set of sentences is
     complete (i.e., covers all states where an error can arise), but this
     may be costly -- it requires running [LRijkstra]. Instead, we offer a
     separate facility for comparing two [.messages] files, one of which can
     be produced via [--list-errors]. This can be used to ensure
     completeness. *)

  (* Now, compile this information down to OCaml code. We wish to
     produce a function that maps a state number to a message. By
     convention, we call this function [message]. *)
  compile_runs filename runs

(* --------------------------------------------------------------------------- *)

(* If two [--compare-errors <filename>] directives are provided, compare the
   two message descriptions files, and stop. We wish to make sure that every
   state that appears on the left-hand side appears on the right-hand side as
   well. *)

let compare_errors filename1 filename2 =

  (* Read both files. *)

  let runs1 = read_messages `Lax filename1
  and runs2 = read_messages `Lax filename2 in

  (* Convert the right-hand file to a table for quick lookup. *)

  let table2 = message_table false runs2 in

  (* There is no need to convert the left-hand file. In fact, not
     converting it to a table allows us to produce error messages
     in an order that respects the left-hand file. Indeed, the
     left-hand file is processed by the following loop: *)

  Report.monitor `Strict @@ fun c ->
  foreach_targeted_sentence runs1 @@ fun (sentence1, target1) message1 ->
  let s = target2state target1 in

  (* 1. Check that the target state [s] appears in [table2]. *)

  match Lr1.NodeMap.find s table2 with

  | exception Not_found ->
      let poss1 = ranges sentence1 in
      Report.signal c poss1
        "this sentence leads to an error in state %d.\n\
         No sentence that leads to this state exists in \"%s\"."
        (Lr1.encode s) filename2

  (* 2. Check that [s] is mapped by [table1] and [table2] to the same
     error message. As an exception, if the message found in [table1] is
     the default message, then no comparison takes place. This allows
     using [--list-errors] and [--compare-errors] in conjunction to ensure
     that a [.messages] file is complete, without seeing warnings about
     different messages. *)

  | sentence2, message2 ->
      if message1 <> default_message && message1 <> message2 then begin
        let poss1 = ranges sentence1
        and poss2 = ranges sentence2 in
        Report.warning c (poss1 @ poss2)
          "these sentences lead to an error in state %d.\n\
           The corresponding messages in \"%s\" and \"%s\" differ."
          (Lr1.encode s) filename1 filename2
      end

(* --------------------------------------------------------------------------- *)

(* Auxiliary functions for [merge_errors]. *)

(* [is_blank c] determines whether the comment [c] is blank. *)

let is_blank_char c =
  match c with
  | ' ' | '\n' | '\r' | '\t' ->
      true
  | _ ->
      false

let rec is_blank c i n =
  i = n || is_blank_char c.[i] && is_blank c (i+1) n

let is_blank c =
  is_blank c 0 (String.length c)

(* [remove_leading_blank_comment] removes a leading blank comment
   from a list. *)

let remove_leading_blank_comment xs =
  match xs with
  | [] ->
      []
  | Comment c :: xs when is_blank c ->
      xs
  | _ :: xs ->
      xs

(* A simple queue where [emit] inserts an element at the end and [elements]
   returns the current list of all elements and clears the queue. *)

module Q = struct

  let create () =
    let q = ref [] in
    let emit x =
      q := x :: !q
    and elements () =
      let xs = List.rev !q in
      q := [];
      xs
    in
    emit, elements

end

let conflict_comment filename =
  sprintf
    "#@ WARNING:\n\
     #@ The following sentence has been copied from \"%s\".\n\
     #@ It is redundant with a sentence that appears earlier in this file,\n\
     #@ so one of them must be removed.\n"
    filename

let toplevel_comment filename =
  sprintf
    "#@ WARNING:\n\
     #@ The following comment has been copied from \"%s\".\n\
     #@ It may need to be proofread, updated, moved, or removed.\n"
    filename

(* [is_default_run p run] tests whether [run] is a default run, that is, a
   run that consists of a single sentence and a default message. If so, it
   additionally tests whether the sentence's target state satisfies [p]. *)

let is_default_run (p : Lr1.node -> bool) (run : run) =
  run.message = default_message &&
  let sentences : targeted_sentence list =
    List.fold_left (OrComment.fold (fun xs x -> x :: xs)) [] run.elements
  in
  match sentences with
  | [ (_sentence, target) ] ->
      let s = target2state target in
      p s
  | _ ->
      false

(* [remove_default_runs] removes from the list [runs] the default runs
   whose target state satisfies [p]. *)

(* We make the assumption that a default run does not contain interesting
   comments, so it is not a problem to lose these comments when the run
   is removed. *)

let rec remove_default_runs p (runs : run or_comment list) =
  match runs with
  | [] ->
      []
  | Comment c :: runs ->
      Comment c :: remove_default_runs p runs
  | Thing run :: runs ->
      if is_default_run p run then
        remove_default_runs p (remove_leading_blank_comment runs)
      else
        Thing run :: remove_default_runs p runs

(* [keep_default_runs] keeps from the list [runs] just the default runs. *)

let keep_default_runs (runs : run or_comment list) =
  List.flatten (List.map (function
  | Comment _ ->
      []
  | Thing run ->
      if is_default_run (fun _ -> true) run then
        [ Thing run ]
      else
        []
  ) runs)

(* [targets run] is the set of target states of a run. *)

let targets (run : run) : Lr1.NodeSet.t =
  List.fold_left (OrComment.fold (fun states (_, target) ->
    let s = target2state target in
    Lr1.NodeSet.add s states
  )) Lr1.NodeSet.empty run.elements

(* [insert_runs inserts runs] inserts the content of the table [insert] into
   the list [runs] at appropriate points that are determined by the target
   states. *)

let insert_runs
    (inserts : run or_comment list Lr1.NodeMap.t)
    (runs : run or_comment list)
  : run or_comment list =

  let emit, emitted = Q.create() in
  runs |> List.iter begin function
  | Thing run ->
      (* Emit this run. *)
      emit (Thing run);
      (* Then, check if the states reached by the sentences in this run appear
         in the table [inserts]. If so, emit the corresponding data. *)
      targets run |> Lr1.NodeSet.iter begin fun s ->
        match Lr1.NodeMap.find s inserts with
        | data ->
            List.iter emit data
        | exception Not_found ->
            ()
      end
  | Comment c ->
      emit (Comment c)
  end;
  emitted()

(* [gather_followers] turns a list of things and comments into a list of
   things-followed-with-comments. Any leading comments are silently lost. *)

let rec gather_followers (xs : 'a or_comment list) : ('a * comment list) list =
  match xs with
  | Comment _ :: xs ->
      (* If there is a leading comment, ignore it. I believe that in a list
         of sentences, our current lexer never produces a leading comment.
         Indeed, a leading comment would be considered part of the previous
         toplevel comment. *)
      gather_followers xs
  | Thing x :: xs ->
      gather_followers_thing x [] xs
  | [] ->
      []

and gather_followers_thing x cs xs =
  match xs with
  | Comment c :: xs ->
      gather_followers_thing x (c :: cs) xs
  | _ ->
      (x, List.rev cs) :: gather_followers xs

(* [space xs] ensures that every thing is followed with a least one newline.
   If that is not the case, a blank line is inserted. This is unpleasant, but
   I have difficulty dealing with my own baroque file format. *)

let has_leading_newline = function
  | Comment c ->
      assert (c <> "");
      c.[0] = '\n'
  | Thing _ ->
      false

let rec space (xs : 'a or_comment list) : 'a or_comment list =
  match xs with
  | [] ->
      []
  | Thing x1 :: x2 :: xs when not (has_leading_newline x2) ->
      Thing x1 :: Comment "\n" :: space (x2 :: xs)
  | x :: xs ->
      x :: space xs

(* --------------------------------------------------------------------------- *)

(* If two [--merge-errors <filename>] directives are provided, compare the two
   message descriptions files and produce a merged .messages file. *)

(* The code is modeled after [compare_errors] above. When we find that an
   entry exists on the left-hand side yet is missing on the right-hand side,
   we note that it should be added. *)

(* If multiple sentences on the left-hand side share an error message, we
   attempt to preserve this feature when these sentences are copied to the
   right-hand side. This prevents us from using [foreach_targeted_sentence];
   we use two nested loops instead. *)

(* If the target state of a sentence on the left-hand side does not exist on
   the right-hand side, then this sentence/message pair is inserted at the end
   of the right-hand side.

   If the target state of a sentence on the left-hand side exists also on the
   right-hand side, albeit with a different message, then the left-hand
   sentence/message pair must be inserted into the right-hand side at a
   suitable position (that is, after the sentence/message pair that already
   exists on the right-hand side). Furthermore, if the sentence/message pair
   on the right-hand side involves the default message, then it should be
   removed and replaced. *)

let merge_errors filename1 filename2 =

  let runs1 = read_messages `Lax filename1
  and runs2 = read_messages `Lax filename2 in

  (* Remove the default runs on the right-hand side whose target state also
     appears on the left-hand side. We lose no information in doing so. *)
  let table1 = message_table false runs1 in
  let covered1 s = Lr1.NodeMap.mem s table1 in
  let runs2 = remove_default_runs covered1 runs2 in

  (* Remove the default runs on the left-hand side whose target state also
     appears on the right-hand side. Again, we lose nothing in doing so. *)
  let table2 = message_table false runs2 in
  let covered2 s = Lr1.NodeMap.mem s table2 in
  let runs1 = remove_default_runs covered2 runs1 in

  (* The default runs that remain on either side are unique. Set them aside,
     to be copied at the end. *)
  let default1 = keep_default_runs runs1
  and default2 = keep_default_runs runs2
  and runs1 = remove_default_runs (fun _ -> true) runs1
  and runs2 = remove_default_runs (fun _ -> true) runs2 in

  (* Use [append] when a run must be appended at the end. *)
  let (append : run or_comment -> unit), appended =
    Q.create()
  in

  (* Use [insert] when a run must be inserted at a specific point. *)
  let inserts : run or_comment list Lr1.NodeMap.t ref =
    ref Lr1.NodeMap.empty in

  let insert (s : Lr1.node) (newer : run or_comment list) =
    let earlier =  try Lr1.NodeMap.find s !inserts with Not_found -> [] in
    inserts := Lr1.NodeMap.add s (earlier @ newer) !inserts
  in

  runs1 |> List.iter begin fun entry ->
  match entry with

  | Comment c ->
      (* We do not want to lose the toplevel comments in the left-hand
         file, so we append them. This is not great, as they may become
         badly placed. We cannot really do better, though, as we do not
         know with what sentence they should be attached. (It may even
         be the case that they should be split and attached partly with
         the previous sentence and partly with the next one.) *)
      if not (is_blank c) then begin
        append (Comment (toplevel_comment filename1));
        append entry
      end

  | Thing run1 ->

    let message1 = run1.message in
    assert (message1 <> default_message);

    (* The sentences in the queue [retained] are to be associated with
       [message1], forming a run, which is to be inserted at the end. *)
    let retain, retained = Q.create() in

    (* The fact that [run1.elements] is a mixture of sentences and comments is
       problematic. We do not know which comments are intended to be paired
       with which sentences. We adopt the convention that a comment is
       associated with the sentence that precedes it. The auxiliary
       function [gather_followers] helps us follow this convention. *)

    run1.elements
    |> gather_followers
    |> List.iter begin fun ((sentence1, target1), comments) ->

        let comments = List.map (fun c -> Comment c) comments in
        let s = target2state target1 in
        match Lr1.NodeMap.find s table2 with

        | exception Not_found ->

            (* This sentence is missing on the right-hand side, so this pair
               of a sentence and message must be retained. The accompanying
               comments are preserved. *)
            retain (Thing (sentence1, target1));
            List.iter retain comments

        | _sentence2, message2 ->
            assert (message2 <> default_message);
            if message1 <> message2 then begin

              (* This sentence exists on the right-hand side, with a different
                 message, so this sentence and message must be inserted in the
                 right-hand side. We construct a singleton run (consisting of
                 just one sentence and one message) and schedule it for
                 insertion. If this sentence was part of a group of several
                 sentences that share a message, then this sharing is lost.
                 Preserving it would be difficult. The user can manually
                 recreate it if desired. *)

              let c = conflict_comment filename1 in
              let elements = Thing (sentence1, target1) :: comments in
              let run = { run1 with elements } in
              insert s [Comment c; Thing run]

            end

    end; (* end of the loop over the elements of this run *)

    (* If the queue [retained] is nonempty, then all of the sentences in it
       must be associated with [message1], forming a run, which must be
       inserted at the end. *)

    let retained = retained() in
    if retained <> [] then begin
      let elements = retained in
      let run = { run1 with elements } in
      append (Thing run)
    end

  end; (* end of the loop over runs *)

  (* The new data is constructed as follows: *)

  let runs =
    (* The non-default runs in [runs2], into which we insert some runs
       from [run1]. *)
    insert_runs !inserts runs2 @
    (* The non-default runs from [runs1] that we have decided to append
       at the end. *)
    appended() @
    (* The default runs from both sides. *)
    default1 @
    default2
  in

  (* Print. *)

  List.iter write_run (space runs)

(* --------------------------------------------------------------------------- *)

(* If [--update-errors <filename>] is set, update the error message
   descriptions found in file [filename]. The idea is to re-generate
   the auto-comments, which are marked with ##, while leaving the
   rest untouched. *)

let update_errors filename =
  (* Read the file. *)
  let runs : run or_comment list = read_messages `Lax filename in

  (* We might wish to detect if two sentences lead to the same state. We
     might also wish to detect if this set of sentences is incomplete,
     and complete it automatically. However, the first task is carried
     out by [--compile-errors] already, and the second task is carried
     out by [--list-errors] and [--compare-errors] together. For now,
     let's try and keep things as simple as possible. The task of
     [--update-errors] should be to update the auto-generated comments,
     without failing, and without adding or removing sentences. *)

  (* Now, write a new [.messages] to the standard output channel, with
     new auto-generated comments. *)
  List.iter write_run runs

(* --------------------------------------------------------------------------- *)

(* If [--echo-errors <filename>] is set, echo the error sentences found in file
   [filename]. Do not echo the error messages or the comments. *)

(* In principle, we should able to run this command without even giving an .mly
   file name on the command line, and without building the automaton. This is
   not possible at the moment, because our code is organized in too rigid a
   manner. *)

let echo_errors filename =
  (* Read the file. *)
  let runs : run or_comment list = read_messages `Lax filename in
  (* Echo. *)
  foreach_targeted_sentence runs @@ fun ((_, sentence), _target) _message ->
  print_string (Sentence.print `Abstract sentence)

(* [--echo-errors-concrete] works like [--echo-errors], except every sentence
   is followed with an auto-generated comment that shows its concrete syntax. *)

let echo_errors_concrete filename =
  (* Read the file. *)
  let runs : run or_comment list = read_messages `Lax filename in
  (* Echo. *)
  foreach_targeted_sentence runs @@ fun ((_, sentence), _target) _message ->
  print_string (Sentence.print `Abstract sentence);
  if Terminal.every_token_has_an_alias then
    printf
      "## Concrete syntax: %s\n"
      (Sentence.print `Concrete sentence)
