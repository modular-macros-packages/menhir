(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module constructs the interface (that is, the [.mli] file)
   of the generated parser. It also provides the definition of the
   internal submodule [Basics]. *)

open IL
open PlainSyntax

module Make (X : sig

  (**[comment] determines whether comments should be included in the
     OCaml code that is generated. *)
  val comment: bool

  (**[exn_carries_state] determines whether the exception [Error] should carry
     an argument, which is a state number. This is the state where a syntax
     error is detected. This feature is supported only by the code back-end. *)
  val exn_carries_state: bool

  (**[fixedexc] determines whether the exception [Error] should be declared
     equal to the exception [Parsing.Parse_error]. Setting this flag to [true]
     can be useful when full compatibility with ocamlyacc is desired. *)
  val fixedexc: bool

  (**If [mode] is [`DefineTokenType] then the types [token] and ['a terminal]
     are defined as algebraic data types. If it is [`UseExternalTokenType "M"]
     then they are defined as abbreviations for pre-existing types [M.token]
     and ['a M.Interpreter.terminal]. *)
  val mode: [ `DefineTokenType | `UseExternalTokenType of string]

  (**[incremental] determines whether the incremental API must be generated.
     This flag is [true] if and only if the table back-end has been selected. *)
  val incremental: bool

  (**[inspection] determines whether the inspection API must be generated. This
     API involves GADTs, and requires producing more tables, so this flag is off
     by default. This flag can be [true] only if the table back-end has been
     selected. In other words, [inspection] implies [incremental]. The
     inspection API is an extension of the incremental API. *)
  val inspection : bool

  (**[unparsing] determines whether the unparsing API must be generated. This
     API offers facilities for constructing and displaying concrete syntax
     trees. This flag can be [true] only if the table back-end has been
     selected. In other words, [unparsing] implies [incremental]. *)
  val unparsing : bool

end) : sig

  (**[incremental] is the the name of the submodule that contains the
     entry points of the incremental API. *)
  val incremental: string

  (**[checkpoint] constructs an application of the parameterized type
     ['a checkpoint], which is defined in the submodule [Interpreter].
     This type exists in the incremental API only. *)
  val checkpoint: typ -> typ

  (**[write grammar filename ()] writes the interface of the generated
     parser to the [.mli] file named [filename]. *)
  val write: grammar -> string -> unit -> unit

end (* Make *)

(**The internal submodule [Basics] exists both when the code back-end
   is used and when the table back-end is used. *)
module Basics (X : sig

  (**[exn_carries_state] determines whether the exception [Error] should carry
     an argument, which is a state number. This is the state where a syntax
     error is detected. This feature is supported only by the code back-end. *)
  val exn_carries_state: bool

  (**[fixedexc] determines whether the exception [Error] should be declared
     equal to the exception [Parsing.Parse_error]. Setting this flag to [true]
     can be useful when full compatibility with ocamlyacc is desired. *)
  val fixedexc: bool

  (**If [mode] is [`DefineTokenType] then the types [token] and ['a terminal]
     are defined as algebraic data types. If it is [`UseExternalTokenType "M"]
     then they are defined as abbreviations for pre-existing types [M.token]
     and ['a M.Interpreter.terminal]. *)
  val mode: [ `DefineTokenType | `UseExternalTokenType of string]

end) : sig

  (**A toplevel function [stop] raises the exception [Error]. *)

  (**If [exn_carries_state] is [true], the exception [Error] carries an integer
     parameter, a state number. This state number must be passed as an argument
     to [call_stop]. If [exn_carries_state] is [false] then [call_stop] ignores
     its argument. *)

  (**[call_stop] emits a call to [stop], which raises the exception [Error]. *)
  val call_stop: int -> expr

  (**The name of the internal submodule [Basics]. *)
  val basics: string

  (**The structure items [basics_def grammar] define and include the internal
     submodule [Basics]. This submodule defines the exception [Error], the
     function [stop], and the type [token]. *)
  val basics_def: grammar -> structure

end (* Basics *)
