(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This is the abstract syntax for a plain grammar, that is, a grammar that
   does not have any parameterized nonterminal symbols. *)

(* A plain grammar is obtained as the result of an expansion phase, which
   takes place in several steps, implemented in [SelectiveExpansion] and
   [Drop]. *)

(* ------------------------------------------------------------------------ *)

(* In a plain grammar, %attribute declarations can be desugared away. This is
   also done by [Drop].

   Thus, in a plain grammar, attributes can be attached in the following
   places:

   - with the grammar:          field [gr_attributes] of [grammar]
   - with a terminal symbol:    field [tk_attributes] of [properties]
   - with a nonterminal symbol: field [attributes] of [rule]
   - with a producer:           field [prod_attributes] of [producer]
   - with a branch:             field [br_attributes] of [branch]           *)

open Attribute

(* ------------------------------------------------------------------------ *)

(* We repeat all of the following type definitions here, because we
   want the clients of this module to say just [open PlainSyntax],
   without having to also say [open Syntax]. *)

type range =
  Syntax.range

type 'a located =
  'a Syntax.located

type ocamltype = Syntax.ocamltype =
  | Declared of string located
  | Inferred of string

type terminal =
  Syntax.terminal

type nonterminal =
  Syntax.nonterminal

type symbol =
  Syntax.symbol

type symbols =
  Syntax.symbols

type alias =
  Syntax.alias

type identifier =
  Syntax.identifier

type action =
  Syntax.action

type associativity = Syntax.associativity =
  | LeftAssoc
  | RightAssoc
  | NonAssoc
  | UndefinedAssoc

type precedence_level = Syntax.precedence_level =
  | UndefinedPrecedence
  | PrecedenceLevel of (InputFile.file * int) located

type properties = Syntax.properties =
  {
    tk_ocamltype     : ocamltype option;
    tk_position      : range;
    tk_alias         : alias;
    tk_attributes    : attributes;
    tk_associativity : associativity;
    tk_precedence    : precedence_level;
    tk_is_declared   : bool;
  }

type prec_annotation =
  Syntax.prec_annotation

type production_level = Syntax.production_level =
  | ProductionLevel of InputFile.file * int

type on_error_reduce_level =
  Syntax.on_error_reduce_level

(* ------------------------------------------------------------------------ *)

(**A producer is a pair of a (located) identifier and a symbol. In concrete
   syntax, it could be [e = expr], for instance. It carries a number of
   attributes. *)
type producer =
  {
    prod_id         : identifier located;
    prod_symbol     : symbol;
    prod_attributes : attributes;
  }

type producers =
  producer list

(* ------------------------------------------------------------------------ *)

(**A branch contains a series of producers and a semantic action. It is the
   same as in the surface syntax; see [Syntax]. *)
type branch =
  {
    branch_position  : range;
    producers        : producers;
    action           : action;
    prec_annotation  : prec_annotation;
    production_level : production_level;
    br_attributes    : attributes;
  }

type branches =
  branch list

(* ------------------------------------------------------------------------ *)

(**A rule consists mainly of several branches. In contrast with the surface
   syntax, it has no parameters.

   The [%inline] flag is no longer relevant after %inline symbols have been
   eliminated. *)
type rule =
  {
    branches    : branches;
    positions   : range list;
    inline_flag : bool;
    attributes  : attributes;
  }

(* ------------------------------------------------------------------------ *)

(**A grammar is essentially the same as in the surface syntax; see [Syntax].
   The main difference is that [%attribute] declarations, represented by
   the field [p_symbol_attributes] in the surface syntax, have disappeared. *)
type grammar =
  {
    preludes        : string located list;
    postludes       : string located list;
    parameters      : string located list;
    start_symbols   : StringSet.t;
    types           : ocamltype StringMap.t;
    tokens          : properties StringMap.t;
    on_error_reduce : on_error_reduce_level StringMap.t;
    gr_attributes   : attributes;
    rules           : rule StringMap.t;
  }
