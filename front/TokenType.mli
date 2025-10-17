(**This module constructs the definitions of the type [token],
   which describes the tokens, and of the type [_ terminal],
   which (in the inspection API) describes the terminal symbols. *)

open PlainSyntax
open IL

(**The type [token] describes the tokens. A token contains a tag
   (a terminal symbol) and possibly a semantic value. *)

(**The type ['a terminal] describes the terminal symbols.
   A terminal symbol is just a tag; it does not carry a semantic value. *)

type mode =

  | DefineTokenType
    (**Generate definitions of the type [token] and ['a terminal] as
       algebraic data types. *)

  | UseExternalTokenType of string * string
    (**Generate definitions of the type [token] and ['a terminal] as
       abbreviations for pre-existing types [M.token] and ['a M'.terminal]. *)

module Make (M : sig
  val grammar : grammar
  val mode : mode
end) : sig

(**[tctoken] is the unqualified conventional name of the type [token]. *)
val tctoken: string

(**[ttoken] is the type [token]. *)
val ttoken: typ

(**If [t] is a terminal symbol, then [tokendata t] is the name of the
   corresponding data constructor in the type [token]. If [mode] is
   [UseExternalTokenType (M, _)] then this name is qualified with [M.]. *)
val tokendata: string -> string

(**[tctokengadt] is the unqualified conventional name of the type [terminal],
   which we also refer to as the token GADT. This is an indexed type: it has
   one type parameter. Its data constructors carry zero value arguments. *)
val tctokengadt: string

(**If [a] is a type then [ttokengadt a] is the type [a terminal]. *)
val ttokengadt: typ -> typ

(**If [t] is a terminal symbol, then [tokengadtdata t] is the name of the
   corresponding data constructor of the type [_ terminal]. If [mode] is
   [UseExternalTokenType (_, M')] then this name is qualified with [M'.]. *)
val tokengadtdata: string -> string

(**[tokentypedef] is the definition of the type [token]. If [mode] is
   [DefineTokenType] then it is defined as an algebraic data type. If [mode]
   is [UseExternalTokenType (M, _)] then it is defined as an abbreviation for
   the type [M.token]. *)
val tokentypedef: interface

(**[tokengadtdef] is the definition of the type [_ terminal]. If [mode] is
   [DefineTokenType] then it is defined as a GADT. If [mode] is
   [UseExternalTokenType (_, M')] then it is defined as an abbreviation for
   the type [M'.tokengadt]. *)
val tokengadtdef: interface

end
