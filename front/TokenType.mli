(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module constructs the definitions of the type [token],
   which describes the tokens, and of the type [_ terminal],
   which (in the inspection API) describes the terminal symbols. *)

open PlainSyntax
open IL

(**The type [token] describes the tokens. A token contains a tag
   (a terminal symbol) and possibly a semantic value. *)

(**The type ['a terminal] describes the terminal symbols.
   A terminal symbol is just a tag; it does not carry a semantic value. *)

(**If [mode] is [`DefineTokenType] then the types [token] and ['a terminal]
   are defined as algebraic data types. If it is [`UseExternalTokenType "M"]
   then they are defined as abbreviations for pre-existing types [M.token] and
   ['a M.Interpreter.terminal]. *)
type mode =
  [ `DefineTokenType | `UseExternalTokenType of string]

(**[interpreter] is the name of the [Interpreter] submodule,
   which is generated when the table back-end is used. *)
val interpreter: string

(**[tctoken] is the unqualified conventional name of the type [token]. *)
val tctoken: string

(**[ttoken] is the type [token]. *)
val ttoken: typ

(**If [t] is a terminal symbol, then [tokendata mode t] is the name of the
   corresponding data constructor in the type [token]. If [mode] is
   [UseExternalTokenType M] then this name is qualified with [M.]. *)
val tokendata: mode -> string -> string

(**[tctokengadt] is the unqualified conventional name of the type [terminal],
   which we also refer to as the token GADT. This is an indexed type: it has
   one type parameter. Its data constructors carry zero value arguments. *)
val tctokengadt: string

(**If [a] is a type then [ttokengadt a] is the type [a terminal]. *)
val ttokengadt: typ -> typ

(**If [t] is a terminal symbol, then [tokengadtdata mode t] is the name of the
   corresponding data constructor of the type [_ terminal]. If [mode] is
   [UseExternalTokenType M] then this name is qualified with
   [M.Interpreter.]. *)
val tokengadtdata: mode -> string -> string

(**[tokentypedef mode grammar] is the definition of the type [token] for the
   grammar [grammar]. If [mode] is [DefineTokenType] then it is defined as an
   algebraic data type. If [mode] is [UseExternalTokenType M] then it is
   defined as an abbreviation for the type [M.token]. *)
val tokentypedef: mode -> grammar -> interface

(**[tokengadtdef mode grammar] is the definition of the type [_ terminal]. If
   [mode] is [DefineTokenType] then it is defined as a GADT. If [mode] is
   [UseExternalTokenType M] then it is defined as an abbreviation for the type
   [M.Interpreter.tokengadt]. *)
val tokengadtdef: mode -> grammar -> interface

(**[write base inspection comment mode grammar] creates an [.ml] file and an
   [.mli] file that contain the definitions of the token type and of the token
   GADT for the grammar [grammar] in mode [mode]. The parameter [base] is the
   name of the desired files, without an extension. The Boolean parameter
   [inspection] determines whether the inspection API has been requested by
   the user; the token GADT is generated only in this case. The Boolean
   parameter [comment] determines whether comments should be generated. *)
val write: string -> bool -> bool -> mode -> grammar -> unit
