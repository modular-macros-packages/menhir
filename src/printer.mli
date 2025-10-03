(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers a pretty-printer for the language [IL]. *)

module Make (X : sig

  (* This is the channel that is being written to. *)

  val f: out_channel

  (* [print_line_directives] controls the way we print OCaml source code
     fragments (types and semantic actions). If it is [Some dstfilename],
     where [dstfilename] is the name of the file that is being written, then
     we surround fragments with OCaml line directives of the form # <line
     number> <filename>. If it is [None], then we don't. *)

  (* Providing line directives allows the OCaml typechecker to report type
     errors in the .mly file, instead of in the generated .ml / .mli files.
     Line directives also affect the dynamic semantics of any [assert]
     statements contained in semantic actions: when they are provided, the
     [Assert_failure] exception carries a location in the .mly file. As a
     general rule of thumb, line directives should always be provided, except
     perhaps where we think that they decrease readability (e.g., in a
     generated .mli file). *)

  val print_line_directives: string option

  (* [print_comments] controls whether the comments carried by [EComment]
     are printed or erased. *)

  val print_comments: bool

end) : sig

  val program: IL.program -> unit

  val expr: IL.expr -> unit

  val interface: IL.interface -> unit

end

(* Common instantiations. In the following two functions, [locate]
   is [None], so no line directives are printed. *)

val     print_expr: out_channel -> IL.expr -> unit
val string_of_expr:                IL.expr -> string
