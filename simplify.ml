open Ast

let simplify_not phi =
  match phi with
  | True -> False
  | False -> True
  | UnaryOp (Not, psi) -> psi
  | _ -> UnaryOp (Not, phi)

let simplify_and phis =
  let phis = List.filter (fun phi -> phi <> True) phis in
  if List.mem False phis then False
  else
    match phis with
    | [] -> True
    | [phi] -> phi
    | _ -> NaryOp (And, phis)

let simplify_or phis =
  let phis = List.filter (fun phi -> phi <> False) phis in
  if List.mem True phis then True
  else
    match phis with
    | [] -> False
    | [phi] -> phi
    | _ -> NaryOp (Or, phis)

let simplify_imp phi psi =
  match phi, psi with
  | False, _ -> True
  | True, _ -> psi
  | _, True -> True
  | _, False -> simplify_not phi
  | _ -> BinaryOp (Implies, phi, psi)

let simplify_next phi =
  match phi with
  | True -> True
  | False -> False
  | _ -> UnaryOp (Next, phi)

let simplify_globally phi =
  match phi with
  | True -> True
  | False -> False
  | UnaryOp (Globally, psi) -> phi 
  | _ -> UnaryOp (Globally, phi)

let simplify_eventually phi =
  match phi with
  | True -> True
  | False -> False
  | UnaryOp (Eventually, psi) -> phi 
  | _ -> UnaryOp (Eventually, phi)

let simplify_weak_until phi psi =
  match (phi, psi) with
  | (False, _) -> psi
  | (True, _) -> True
  | (_, True) -> True
  | (_, False) -> UnaryOp(Globally, phi)
  | _ -> BinaryOp(WeakUntil, phi, psi)

let simplify_next_weak_until phi psi =
  match (phi, psi) with
  | (False, _) -> False
  | (True, _) -> True
  | (_, True) -> phi
  | (_, False) -> UnaryOp(Globally, phi)
  | _ -> BinaryOp(NextWeakUntil, phi, psi)

let simplify_release phi psi =
  match (phi, psi) with
  | (False, _) -> UnaryOp(Globally, psi)
  | (True, _) -> psi
  | (_, True) -> True
  | (_, False) -> False 
  | _ -> BinaryOp(Release, phi, psi)

let simplify_next_release phi psi =
  match (phi, psi) with
  | (False, _) -> False
  | (True, _) -> True
  | (_, True) -> UnaryOp (Eventually, phi)
  | (_, False) -> phi 
  | _ -> BinaryOp(NextRelease, phi, psi)

let rec simplify phi =
  match phi with
  | False | True -> phi
  | UnaryOp (Not, psi) ->
      simplify_not (simplify psi)
  | UnaryOp (op, psi) ->
      UnaryOp (op, simplify psi)
  | NaryOp (And, phis) ->
      simplify_and (List.map simplify phis)
  | NaryOp (Or, phis) ->
      simplify_or (List.map simplify phis)

  | BinaryOp (Implies, phi, psi) ->
      simplify_imp (simplify phi) (simplify psi)
  | BinaryOp (op, phi, psi) ->
      BinaryOp (op, simplify phi, simplify psi)

  | Quantified (quantifier, variables, body) ->
      Quantified (quantifier, variables, simplify body)
