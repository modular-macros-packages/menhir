(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Lexing
open MiddleAPI

(* A traditional lexer returns a token and updates the fields of the lexing
   buffer with new positions. Here, we have no position information, so we
   keep the dummy positions that exist at initialization time. *)

(* Once the finite list is exhausted, two plausible behaviors come to mind.

   One behavior consists in raising an exception. In that case, we are creating
   a finite stream, and it is up to the parser to not read past its end.

   Another behavior consists in returning a designated token. In that case, we
   are creating an infinite, eventually constant, stream.

   The choice between these two behaviors is somewhat arbitrary; furthermore,
   in the second case, the choice of the designated token is arbitrary as
   well. Here, we adopt the second behavior if and only if the grammar has an
   EOF token, and we use EOF as the designated token. Again, this is
   arbitrary, and could be changed in the future. *)

module Make (G : GRAMMAR) = struct
open G

exception EndOfStream

let stream ts =
  let ts = ref ts in
  let lexbuf = from_string "" in
  lexbuf.lex_start_p <- dummy_pos;
  lexbuf.lex_curr_p <- dummy_pos;
  let lexer _lexbuf : Terminal.t =
    match !ts with
    | tok :: more ->
        ts := more;
        tok
    | [] ->
        match Terminal.eof with
        | Some eof ->
            eof
        | None ->
            raise EndOfStream
  in
  lexer, lexbuf

end
