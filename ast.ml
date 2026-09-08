(** Abstract Syntax Tree for the Imp language *)

type location = Lexing.position * Lexing.position

type identifier = {
  location : location;
  name : string;
}

type unary_expr_op =
  | Succ

type binary_expr_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Equal
  | LessEqual
  | LessThan

type constant =
  | Zero
  | One

type expression =
  | Const of constant
  | Variable of identifier
  | Unary of unary_expr_op * expression
  | Binary of binary_expr_op * expression * expression

type unary_prop_op =
  | Not
  | Next        (* X *)
  | Globally    (* G *)
  | Eventually  (* F *)

type binary_prop_op =
  | Implies
  | Until         (* U  *) 
  | WeakUntil     (* W  *)
  | NextWeakUntil (* XW *)
  | Release       (* R  *)
  | NextRelease   (* XR *)

type nary_prop_op =
  | And
  | Or

type quantifier =
  | Forall
  | Exists

(* Missing Bind, for now *) 
type proposition =
  | False
  | True
  | Now of expression
  | Omega of int * expression list
  | UnaryOp of unary_prop_op * proposition
  | NaryOp of nary_prop_op * proposition list
  | BinaryOp of binary_prop_op * proposition * proposition
  | Quantified of quantifier * identifier list * proposition
  | Equality of expression * expression

type specification =
  | WellFounded
  | LeastFixedPoint
  | GreatestFixedPoint

type statement =
  | Skip
  | Assign of identifier * expression
  | Assume of proposition
  | Choice of statement list * statement list
  | Iterate of specification list * statement list
  | While of specification list * expression * statement list
  | If of expression * statement list * statement list

type program = statement list

module VarMap = Map.Make(struct
  type t = identifier

  let compare x y =
    String.compare x.name y.name
end)

type subst = expression VarMap.t
type ren = identifier VarMap.t

module Subst = struct

  let empty : subst = VarMap.empty

  let from xs es : subst =
    if List.length xs <> List.length es then
      invalid_arg "Subst.from: lists have different lengths";

    List.fold_left2 (fun su x e -> VarMap.add x e su) VarMap.empty xs es

  let update (su : subst) xs es : subst =
    if List.length xs <> List.length es then
      invalid_arg "Subst.update: lists have different lengths";

    List.fold_left2 (fun su x e -> VarMap.add x e su) su xs es

  let eqs (su : subst) : proposition list =
    List.map (fun (x, e) -> Equality (Variable x, e)) (VarMap.bindings su)

  let now (su : subst) : proposition list =
    List.map (fun eq -> UnaryOp (Next, eq)) (eqs su)

end
