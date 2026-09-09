open Ast
open Util
open Unfold

let rec wp xs (prog: program) cont post =
  match prog with
  | [] -> BinaryOp (Implies, cont, post)
  | Assume phi :: rest ->
    let su = Subst.empty in
    let post = unfold post su in
    let psi = wp xs rest cont post in
    BinaryOp (Implies, phi, psi)
  | Assign (ys, es) :: rest ->
    let ys' = List.init (List.length ys) (fun _ -> fresh_ident ()) in
    let ys' = List.map (fun x -> Variable x) ys' in
    let su = Subst.from ys ys' in
    (* TODO: where do we get this renaming environment from? is it fresh? *)
    let re = Ren.empty in
    let post = unfold post su in
    let rest_ = rename_prog re rest in
    let cont_ = rename_prop re cont in
    let eqs = zip ys es in
    let eqs = List.map (fun (y, e) -> Now (Binary (Equal, Variable y, e))) eqs in
    let phi = NaryOp (And, eqs) in
    (* TODO: Gidon had rest instead of rest_, check this *)
    let psi = wp xs rest_ cont_ post in
    BinaryOp (Implies, phi, psi)
  | Choice (left, right) :: rest ->
    let left_ = wp xs (left @ rest) cont post in
    let right_ = wp xs (right @ rest) cont post in
    NaryOp (And, [left_; right_]) 
  | Skip :: rest -> wp xs rest cont post
  | _ -> assert false
    
     
let vcgen xs prog cont post =
  let post = wp xs prog cont post in
  Quantified (Forall, [], post)

