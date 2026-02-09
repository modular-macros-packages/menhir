type binop =
  | Add
  | Mul

type expr =
  | ELeaf
  | EBinOp of expr * binop * expr
  | EDisj of expr * expr
  | EParen of expr

let smart_binop e1 op e2 =
  match e1, op, e2 with
  (* The children of a multiplication cannot be additions. *)
  | EBinOp (_, Add, _), Mul, _
  | _, Mul, EBinOp (_, Add, _)
  (* The left child of a multiplication cannot be a multiplication. *)
  | EBinOp (_, Mul, _), Mul, _
  (* The left child of an addition cannot be an addition. *)
  | EBinOp (_, Add, _), Add, _
    ->
      Auxiliary.reject()
  | _, _, _ ->
      EBinOp (e1, op, e2)

let smart_lhs e1 op =
  match e1, op with
  (* The left child of a multiplication cannot be an addition. *)
  | EBinOp (_, Add, _), Mul
  (* The left child of a multiplication cannot be a multiplication. *)
  | EBinOp (_, Mul, _), Mul
  (* The left child of an addition cannot be an addition. *)
  | EBinOp (_, Add, _), Add
    ->
      Auxiliary.reject()
  | _, _ ->
      e1, op
