(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Grammar
module C = Conflict (* artificial dependency; ensures that [Conflict] runs first *)

(* Here is how we check whether state [s] should have a default reduction.

   We check whether [s] has no outgoing shift transitions and only has
   one possible reduction action. In that case, we produce a default
   reduction action, that is, we perform reduction without consulting
   the lookahead token. This saves code, but can alter the parser's
   behavior in the presence of errors.

   The check for default actions subsumes the check for the case where
   [s] admits a reduce action with lookahead symbol "#". In that case,
   it must be the only possible action -- see
   [Lr1.default_conflict_resolution]. That is, we have reached a point
   where we have recognized a well-formed input and are now expecting
   an end-of-stream. In that case, performing reduction without
   looking at the next token is the right thing to do, since there
   should in fact be none. The state that we reduce to will also have
   the same property, and so on, so we will in fact end up rewinding
   the entire stack and accepting the input when the stack becomes
   empty.

   (New as of 2012/01/23.) A state where a shift/reduce conflict was
   solved in favor of neither (due to a use of the %nonassoc
   directive) must not perform a default reduction. Indeed, this would
   effectively mean that the failure that was requested by the user is
   forgotten and replaced with a reduction. This surprising behavior
   is present in ocamlyacc and was present in earlier versions of
   Menhir. See e.g. http://caml.inria.fr/mantis/view.php?id=5462

   There is a chance that we might run into trouble if the ideas
   described in the above two paragraphs collide, that is, if we
   forbid a default reduction (due to a shift/reduce conflict solved
   by %nonassoc) in a node where we would like to have default
   reduction on "#". This situation seems unlikely to arise, so I will
   not do anything about it for the moment. (Furthermore, someone who
   uses precedence declarations is looking for trouble anyway.)

   Between 2012/05/25 and 2015/09/25, if [--canonical] has been specified,
   then we disallow default reductions on a normal token, because we do not
   want to introduce any spurious actions into the automaton. We do still
   allow default reductions on "#", since they are needed for the automaton to
   terminate properly. From 2015/09/25 on, we again always allow default
   reductions, as they seem to be beneficial when explaining syntax errors. *)

let is_purely_nonterminal transitions =
  SymbolMap.for_all (fun sym _ -> Symbol.is_nonterminal sym) transitions

let count =
  ref 0

let test_default_reduction : Lr1.node -> (Production.t * TerminalSet.t) option =
  Lr1.tabulate @@ fun s ->
    if Lr1.forbid_default_reduction s then
      None
    else
      let reductions = Reductions.reverse (Lr1.reductions s) in
      if ProductionMap.is_singleton reductions
      && is_purely_nonterminal (Lr1.transitions s) then
        let reduction = ProductionMap.choose reductions in
        incr count;
        Some reduction
      else
        None

let () =
  Error.logC 1 @@ fun f ->
  Printf.fprintf f
    "%d out of %d states have a default reduction.\n"
    !count Lr1.n

let () =
  Time.tick "Computing default reductions"

(* ------------------------------------------------------------------------ *)

(* [find_erroneous s zs] finds whether there is a terminal [z] in [zs] such
   that state [s] will initiate an error on the lookahead symbol [z].
   All terminals in [zs] must be real. *)

let find_erroneous s zs : Terminal.t option =
  match test_default_reduction s with
  | Some _ ->
      None
  | None ->
      let transitions = Lr1.transitions s
      and reductions = Lr1.reductions s in
      zs |> TerminalSet.pick @@ fun z ->
      assert (Terminal.real z);
      not (TerminalMap.mem z reductions) &&
      not (SymbolMap.mem (Symbol.T z) transitions)
