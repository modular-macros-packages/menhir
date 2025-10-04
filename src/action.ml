(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Keyword
open IL
open ILConstruction

type t = {

  expr: expr;
  (**The code for this semantic action. *)

  priority: int;
  (**This priority level is used in [Infer] to sort semantic actions.
     Actions with a lower priority level are processed first by OCaml's
     type-checker, so type errors are less likely to be reported inside
     them. Via inlining, several actions can be combined into one; in
     that case, we take a maximum. *)

  semvars: StringSet.t;
  (**The free variables that this semantic action can use in order to
     refer to a semantic value. *)

  keywords  : KeywordSet.t;
  (**The set of keywords that appear in this semantic action. These keywords
     can be thought of as free variables that refer to positions. They must
     be renamed during inlining. *)

}

(* -------------------------------------------------------------------------- *)

(* Constructors. *)

let make priority semvars keywords expr =
  let keywords = KeywordSet.of_list keywords in
  { expr; priority; semvars; keywords }

(* -------------------------------------------------------------------------- *)

(* Building [let x = a1 in a2]. *)

let compose x a1 a2 =
  {
    expr      = blet ([ PVar x, a1.expr ], a2.expr);
    semvars   = StringSet.union a1.semvars (StringSet.remove x a2.semvars);
    keywords  = KeywordSet.union a1.keywords a2.keywords;
    priority  = max a1.priority a2.priority;
  }

(* Building [let p = x in a]. *)

let rec bind p x a =
  {
    expr      = blet ([ p, EVar x ], a.expr);
    semvars   = StringSet.add x (StringSet.diff a.semvars (bv p));
    keywords  = a.keywords;
    priority  = a.priority;
  }

(* We are lazy and write an incomplete and somewhat ad hoc [bv] function for
   IL patterns. See also [NewRuleSyntax.bv]. *)

and bv p =
  bv_aux StringSet.empty p

and bv_aux accu p =
  match p with
  | PWildcard
  | PUnit ->
      accu
  | PVar x ->
      StringSet.add x accu
  | PTuple ps ->
      List.fold_left bv_aux accu ps
  | _ ->
      assert false (* unsupported *)

(* -------------------------------------------------------------------------- *)

(* Accessors. *)

let to_il_expr action =
  action.expr

let[@inline] priority action =
  action.priority

let semvars action =
  action.semvars

let keywords action =
  action.keywords

let has_beforeend action =
  KeywordSet.mem (Position (Before, WhereEnd, FlavorPosition)) action.keywords

let posvars action =
  KeywordSet.fold (fun keyword accu ->
    match keyword with
    | Position (s, w, f) ->
        let x = Keyword.posvar s w f in
        StringSet.add x accu
  ) (keywords action) StringSet.empty

let vars action =
  StringSet.union (semvars action) (posvars action)

(* -------------------------------------------------------------------------- *)

(* Defining a keyword in terms of other keywords. *)

let define keyword keywords f action =
  assert (KeywordSet.mem keyword action.keywords);
  {
    expr      = f action.expr;
    priority  = action.priority;
    semvars   = action.semvars;
    keywords  = KeywordSet.union
                  keywords (KeywordSet.remove keyword action.keywords)
  }

(* -------------------------------------------------------------------------- *)

(* Simultaneous substitutions are represented as association lists, where no
   name appears twice in the domain. We distinguish renamings that affect
   semantic-value variables and renamings that affect position-keyword
   variables. The two are unfortunately somewhat linked because our
   position-keyword variables are named after semantic-value variables;
   this is quite a mess. *)

type subst =
  {
    semvar: (string * string) list;
    posvar: (string * string) list;
  }

let empty =
  { semvar = []; posvar = [] }

let extend1 x y var =
  assert (not (List.mem_assoc x var));
  if x <> y then (x, y) :: var else var

let extend_semvar x y { semvar; posvar } =
  { semvar = extend1 x y semvar; posvar }

let extend_posvar x y { semvar; posvar } =
  { semvar; posvar = extend1 x y posvar }

let extend = extend_semvar

let apply1 var x =
  try List.assoc x var with Not_found -> x

let apply_semvar phi x =
  apply1 phi.semvar x

let apply_subject phi subject =
  match subject with
  | Before
  | Left ->
      subject
  | RightNamed x ->
      RightNamed (apply_semvar phi x)

let bindings phi =
  phi.posvar @ phi.semvar

let restrict1 xs var =
  List.filter (fun (x, _y) -> StringSet.mem x xs) var

let restrict_semvar xs { semvar; posvar } =
  { semvar = restrict1 xs semvar; posvar }

(* -------------------------------------------------------------------------- *)

(* [rename_keyword f phi keyword] applies [f] to possibly transform the
   keyword [keyword]. If [f] decides to change this keyword (by returning
   [Some _]) then this decision is obeyed. Otherwise, the keyword is renamed
   by the substitution [phi]. In either case, [phi] is extended with a
   renaming decision. *)

let rename_keyword f (phi : subst ref) keyword : keyword =
  match keyword with
  | Position (subject, where, flavor) ->
      let subject', where' =
        match f (subject, where) with
        | Some (subject', where') ->
            subject', where'
        | None ->
            apply_subject !phi subject, where
      in
      phi :=
        extend_posvar
          (Keyword.posvar subject where flavor)
          (Keyword.posvar subject' where' flavor)
          !phi;
      Position (subject', where', flavor)

let rename f phi a =

  (* Rename all keywords, growing [phi] as we go. *)
  let keywords = a.keywords in
  let phi = ref phi in
  let keywords = KeywordSet.map (rename_keyword f phi) keywords in
  let phi = !phi in

  let priority = a.priority in

  (* Restrict [phi.semvar] to the set [a.semvars], in order to avoid
     generating [let] bindings that would be both useless and harmful:
     if [x] is not free in [e], then we do not want to generate
     [let x = x' in e], as suddenly [x'] would be free in this new
     expression. *)
  let phi = restrict_semvar a.semvars phi in

  (* Apply [phi.semvar] to the set of free variables. *)
  let semvars = StringSet.map (apply_semvar phi) a.semvars in

  (* Construct a new semantic action, where [phi] is translated into
     a set of *simultaneous* [let] bindings. (We cannot use a series
     of nested [let] bindings, as that would cause a capture if the
     domain and codomain of [phi] have a nonempty intersection.) *)
  let phi = List.map (fun (x, y) -> PVar x, EVar y) (bindings phi) in
  let expr = eletand (phi, a.expr) in

  { expr; priority; semvars; keywords }
