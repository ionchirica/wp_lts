open Ast

let not phi =
  match phi with
  | Ptrue -> Pfalse
  | Pfalse -> Ptrue
  | Punop (Unot, psi) -> psi
  | _ -> Punop (Unot, phi)

let imp phi psi =
  match (phi, psi) with
  | (Pfalse, _) -> Ptrue
  | (Ptrue, _) -> psi
  | (_, Ptrue) -> Ptrue
  | (_, Pfalse) -> Punop (Unot, phi)
  | _ -> Pbop (Bimp, phi, psi) 

let simplify p =
  match p with
  | Pfalse
  | Ptrue
  | Punop (_, _)
  | Pnop (_, _)
  | Pbop (_, _, _) -> p 
