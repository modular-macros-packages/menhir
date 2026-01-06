{
  open Parser

  exception Error of string
}

(* This rule analyzes a single line and turns it into a stream of
   tokens. *)

rule token = parse
| [' ' '\t']
    { token lexbuf }
| '\n'
    { EOL }
| 'A'
    { A }
| '+'
    { ADD }
| '*'
    { MUL }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
| eof
    { raise (Error (Printf.sprintf "At offset %d: unexpected end of input.\n" (Lexing.lexeme_start lexbuf))) }
