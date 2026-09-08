open Ast

let rec subst_expr su expr =
  match expr with
  | Const c -> Const c
  | Variable x ->
      (match VarMap.find_opt x su with
       | Some e -> e
       | None -> Variable x)
  | Unary (op, e) ->
      Unary (op, subst_expr su e)
  | Binary (op, e1, e2) ->
      Binary (op, subst_expr su e1, subst_expr su e2)

let rec subst_prop su prop =
  match prop with
  | False -> False
  | True -> True
  | Now e -> Now (subst_expr su e)
  | Omega (index, ps) -> Omega (index, List.map (subst_expr su) ps)
  | Equality (e1, e2) ->
      Equality (subst_expr su e1, subst_expr su e2)
  | UnaryOp (op, p) ->
      UnaryOp (op, subst_prop su p)
  | NaryOp (op, ps) ->
      NaryOp (op, List.map (subst_prop su) ps)
  | BinaryOp (op, p1, p2) ->
      BinaryOp (op, subst_prop su p1, subst_prop su p2)
  | Quantified (q, vars, p) ->
      Quantified (q, vars, subst_prop su p)
