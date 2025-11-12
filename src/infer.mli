(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open PlainSyntax
open FrontAPI
open MiddleAPI

module Make (X : sig
  include API_SETTINGS
  include BASE_SETTINGS
  include COMMENT_SETTINGS
  include EXN_SETTINGS
  include INFER_SETTINGS
  include TOKEN_TYPE_SETTINGS
end)
: sig

(* [infer grammar] analyzes the grammar [grammar] and returns a new
   grammar, augmented with a [%type] declaration for every nonterminal
   symbol. The [ocamlc] compiler is used to infer types. *)

val infer: grammar -> grammar

(* [depend postprocess grammar] prints (on the standard output channel) the
   OCaml dependencies induced by the semantic actions. If [postprocess] is
   [true], then ocamldep's output is postprocessed, otherwise it is echoed
   unchanged. *)

val depend: bool -> grammar -> unit

(* [write_query filename grammar] writes the grammar's semantic actions to a
   mock [.ml] file named [filename]. This file can then be submitted to
   [ocamlc] for type inference. See [--infer-write-query <filename>] in the
   manual. *)

val write_query: string -> grammar -> unit

(* [read_reply filename grammar] reads the types inferred by OCaml for the
   mock [.ml] file described above, and returns a new grammar, augmented with
   a [%type] declaration for every nonterminal symbol. *)

val read_reply: string -> grammar -> grammar

end
