open Ast
open Util

let next prop su =
  subst_prop su prop

let rec unfold prop su =
  match prop with
  | True | False | Now (_) | Omega (_) -> prop

  | UnaryOp (Globally, UnaryOp(Eventually, _)) 
  | UnaryOp (Eventually, UnaryOp(Globally, _)) -> next prop su 

  | UnaryOp (Globally, phi) -> NaryOp (And, [unfold phi su; next prop su])

  | UnaryOp (Eventually, phi) -> NaryOp (Or, [unfold phi su; next prop su])

  | BinaryOp (Until, phi, psi)
  | BinaryOp (WeakUntil, phi, psi) -> NaryOp (Or, [unfold psi su; NaryOp (And, [unfold phi su; next prop su])])

  | BinaryOp (NextWeakUntil, phi, psi) -> NaryOp (And, [unfold phi su; next (NaryOp (Or, [psi; prop])) su])

  | BinaryOp (NextRelease, phi, psi) -> NaryOp (Or, [unfold phi su; next (NaryOp (And, [psi; prop])) su])
  | BinaryOp (Release, phi, psi) -> NaryOp (And, [unfold phi su; NaryOp(Or, [unfold phi su; next prop su])])

  | UnaryOp (Not, phi) -> UnaryOp (Not, unfold phi su) 
    
  | BinaryOp (Implies, phi, psi) -> BinaryOp (Implies, unfold phi su, unfold psi su) 

  | NaryOp (And, props) -> NaryOp (And, List.map (fun x -> unfold x su) props)
  | NaryOp (Or, props) -> NaryOp (Or, List.map (fun x -> unfold x su) props)

  | _ -> prop
