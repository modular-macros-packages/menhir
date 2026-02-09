open Printf
open Lexing
open Lexer
open Parser

(* -------------------------------------------------------------------------- *)

(* Offer a choice of input generators. *)

(* [aaaaa n] generates the string "AAA...AAA\n". *)

let rec aaaaa n =
  if n = 1 then "A\n" else "A" ^ aaaaa (n-1)

(* [additions n] generates the string "A+A+...+A+A\n". *)

let rec additions n =
  if n = 1 then "A\n" else "A+" ^ additions (n-1)

(* [random_operations n] generates a random combination of additions,
   multiplications, and parentheses. *)

let split n =
  let k = Random.int (n+1) in
  k, n-k

let rec random_operations n =
  if n <= 2 then "A"
  else
    let draw = Random.int 10 in
    if draw = 0 then
      (* About 10% of chances of inserting a pair of parentheses.
         The more parentheses we insert, the faster the parsing. *)
      "(" ^ random_operations (n-2) ^ ")"
    else if draw <= 5 then
      let n1, n2 = split (n-1) in
      random_operations n1 ^ "+" ^ random_operations n2
    else
      let n1, n2 = split (n-1) in
      random_operations n1 ^ "*" ^ random_operations n2

let random_operations n =
  random_operations n ^ "\n"

(* -------------------------------------------------------------------------- *)

(* Selecting a parser and generator. *)

let selected =
  ref false

let check_selected () =
  if not !selected then begin
    eprintf "Error: no grammar has been selected.\n%!";
    exit 1
  end

let parser : ((lexbuf -> token) -> lexbuf -> unit) ref =
  ref @@ fun _lexer _lexbuf -> ()

let generator : (int -> string) ref =
  ref @@ fun _n -> ""

let set_grammar name =
  begin match name with
  | "arith0" ->
      parser := arith0;
      generator := additions
  | "arith" ->
      parser := arith;
      generator := random_operations
  | "arithr" ->
      parser := arithr;
      generator := random_operations
  | "arithb" ->
      parser := arithb;
      generator := random_operations
  | "gamma5l" ->
      parser := gamma5l;
      generator := aaaaa
  | "gamma5r" ->
      parser := gamma5r;
      generator := aaaaa
  | _ ->
      eprintf "Error: unknown grammar: %s\n%!" name;
      exit 1
  end;
  selected := true

(* -------------------------------------------------------------------------- *)

(* Running the selected parser. *)

let process (input : string) : unit =
  let lexbuf = from_string input in
  try
    (* Run the parser on this input. *)
    !parser token lexbuf
  with
  | Lexer.Error msg ->
      fprintf stderr "%s%!" msg
  | Parser.Error _ ->
      fprintf stderr "At offset %d: syntax error.\n%!"
        (lexeme_start lexbuf)

(* -------------------------------------------------------------------------- *)

(* Timing. *)

let[@inline] now () =
  let open Unix in
  (times()).tms_utime

(* -------------------------------------------------------------------------- *)

(* Benchmarking. *)

(* The input size at which we start. *)
let min_size =
  ref 20

(* The input size at which we stop. *)
let max_size =
  ref 200

(* The geometric size increment. *)
let increment =
  ref 1.05

let next (n : int) : int =
  let n' = Float.to_int (ceil (!increment *. float n)) in
  if n' > n then n' else n+1

(* [repeat] is the desired number of repetitions for each input.
   We repeat so as to obtain better timing accuracy. *)
let repeat =
  ref 3

(* [regenerate] is the number of inputs that we generate at each size.
   This is useful when the generator is random. *)
let regenerate =
  ref 1

let test (n : int) =
  let input = !generator n in
  let time  = ref 0.0 in
  let cur = Auxiliary.current()
  and rej = !Auxiliary.rejects in
  for _i = 1 to !repeat do
    let start = now() in
    process input;
    let stop = now() in
    time  := !time +. stop -. start
  done;
  let time = !time /. float !repeat
  and space = (Auxiliary.current() - cur) / !repeat
  and rejects = (!Auxiliary.rejects - rej) / !repeat in
  (* Print one row of data: input size, number of allocated nodes, time,
     number of rejects. *)
  printf "%d,%d,%.9f,%d\n%!"
    (String.length input) space time
    (if rejects = 0 then 1 else rejects)
    (* hack: map 0 to 1 to allow log *)

let test () =
  printf "n,nodes,time,rejects\n";
  let n = ref !min_size in
  while !n <= !max_size do
    for _i = 1 to !regenerate do
      test !n
    done;
    n := next !n
  done

(* -------------------------------------------------------------------------- *)

(* Parse the command line. *)

let usage =
  sprintf "Usage: %s <options> <grammar name>" Sys.argv.(0)

let spec =
  Arg.align [

    "--increment",
    Arg.Set_float increment,
    "<float> geometric size increment";

    "--min-size",
    Arg.Set_int min_size,
    "<int> minimum input size";

    "--max-size",
    Arg.Set_int max_size,
    "<int> maximum input size";

    "--regenerate",
    Arg.Set_int regenerate,
    "<int> number of generated input sentences at each input size";

    "--repeat",
    Arg.Set_int repeat,
    "<int> number of runs for each input sentence";

  ]

let () =
  Arg.parse spec set_grammar usage;
  check_selected();
  test()
