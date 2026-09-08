open Ast
open Unfold

let rec wp xs (prog: program) cont post =
  match prog with
  | [] -> BinaryOp (Implies, cont, post)
  | Assume phi :: rest ->
    let su = Subst.empty in
    let post = unfold post su in
    let psi = wp xs rest cont post in
    BinaryOp (Implies, phi, psi)
  | _ -> assert false
    
     
let vcgen xs prog cont post =
  let post = wp xs prog cont post in
  Quantified (Forall, [], post)

