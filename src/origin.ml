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

(* The desired information is computed via a forward data flow analysis. *)

(* The join semi-lattice of properties is as follows. *)

module P = struct

  (* [SingleOrigin s] means that we are reachable via a single entry state
     [s]. [Top] means that we are reachable via multiple entry states. *)
  type property =
    | SingleOrigin of Nonterminal.t
    | Top

  let leq_join p1 p2 =
    match p1, p2 with
    | _, Top
    | Top, _ ->
        Top
    | SingleOrigin start1, SingleOrigin start2 ->
        if Nonterminal.equal start1 start2 then p2 else Top

end

(* The call graph of the [run], [reduce] and [goto] functions. *)

module G = struct

  include P

  type variable =
    | Run of Lr1.node
    | Reduce of Production.t
    | Goto of Nonterminal.t

  type t = variable

  let foreach_root yield =
    (* The entry points are the [run] functions associated with each of
       the entry states. *)
    Lr1.entry |> ProductionMap.iter @@ fun prod node ->
    let nt = Production.get_start prod in
    yield (Run node) (SingleOrigin nt)

  let foreach_successor v origin yield =
    match v with
    | Run node ->
        (* For each transition from [node] to [node'], the function [run node]
           calls the function [run node']. In the case of [goto] transitions,
           this is not a direct call (it goes through [reduce] and [goto]
           functions), but it is nevertheless accounted for here. *)
        Lr1.transitions node |> SymbolMap.iter begin fun _label node' ->
          yield (Run node') origin
        end;
        Lr1.reductions node |> TerminalMap.iter begin fun _tok prods ->
          let prod = MList.single prods in
          yield (Reduce prod) origin
        end
    | Reduce prod ->
        (* A [reduce] function ends with a call to a [goto] function. *)
        let nt = Production.nt prod in
        yield (Goto nt) origin
    | Goto _nt ->
        (* A [goto] function appears to make no calls. The calls that it
           makes have already been accounted for above. *)
        ()

end

(* Run the analysis on demand. *)

let solution : (G.variable -> P.property option) Lazy.t =
  lazy (
    let module D = Fix.DataFlow.ForType(G)(P)(G) in
    Time.tick "Computing origins";
    D.solution
  )

(* Convert a [property option] to something clearer for the end user. *)

type origin =
  | Dead
  | SingleOrigin of Nonterminal.t
  | MultipleOrigins

let convert op =
  match op with
  | None ->
      Dead
  | Some (P.SingleOrigin nt) ->
      SingleOrigin nt
  | Some (P.Top) ->
      MultipleOrigins

(* Publish the data. *)

let run node =
  convert (Lazy.force solution (G.Run node))

let reduce prod =
  convert (Lazy.force solution (G.Reduce prod))

let goto nt =
  convert (Lazy.force solution (G.Goto nt))
