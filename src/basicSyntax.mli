open Attribute
open PlainSyntax
val prod_id : producer -> identifier
val prod_attributes : producer -> attributes
val tokens : grammar -> terminal list
val typed_tokens : grammar -> (terminal * ocamltype option) list
val ocamltype_of_token : grammar -> symbol -> ocamltype option
val unquoted_alias : grammar -> symbol -> alias
val nonterminals : grammar -> nonterminal list
val is_nonterminal : grammar -> symbol -> bool
val ocamltype_of_symbol : grammar -> symbol -> ocamltype option
val ocamltype_of_start_symbol : grammar -> symbol -> ocamltype
val is_inline_symbol : grammar -> symbol -> bool
val is_inline_producer : grammar -> producer -> bool
val names : producers -> StringSet.t
val print_production : nonterminal -> branch -> string
val error_free : branch -> bool
