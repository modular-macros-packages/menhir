(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* The code generator. *)

open Grammar
open IL
open CodeBits
open CodePieces

(* The [assertfalse] function. We have just one of these, in order to
   save code size. It should become unnecessary when we add GADTs. *)

let assertfalse =
  prefix "fail"

(* The [print_token] function. This automatically generated function
   is used in [--trace] mode. *)

let print_token =
  prefix "print_token"

(* ------------------------------------------------------------------------ *)

(* Code production for a few auxiliary functions. *)

(* This is [assertfalse], used when internal failure is detected.
   This should never happen if our tool is correct. *)

let internal_failure =
  "Internal failure -- please contact the parser generator's developers."

let assertfalsedef = {
  valpublic = false;
  valpat = PVar assertfalse;
  valval =
    EAnnot (
      EFun ([ PUnit ],
        blet (
          [ PUnit, eprintf internal_failure []],
          eassert efalse
        )
      ),
      scheme [ "a" ] (arrow tunit (tvar "a"))
    )
}

(* Calls to [assertfalse]. *)

let call_assertfalse =
  EApp (EVar assertfalse, [ EUnit ])

(* This is [print_token], used to print tokens in [--trace] mode. *)

let printtokendef =
  destructuretokendef
    print_token
    tstring
    false
    (fun tok -> EStringConst (Terminal.print tok))
