open Ast

module Counter = struct

  let index = ref 0 

  let next () = incr index; !index
  
end

let fresh_var () = "var_" ^ (string_of_int (Counter.next ()))

let fresh_ident () = { location = ( Lexing.dummy_pos, Lexing.dummy_pos);
                       name = fresh_var () } 

(* let () = *)
(*   Printf.printf ("%s\n") (fresh_var ()); *)
(*   Printf.printf ("%s\n") (fresh_var ()) *)

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

let rec rename_expr (re : ren) expr =
  match expr with
  | Const c -> Const c
  | Variable x ->
      (match VarMap.find_opt x re with
       | Some x' -> Variable x'
       | None -> Variable x)
  | Unary (op, e) ->
      Unary (op, rename_expr re e)
  | Binary (op, e1, e2) ->
      Binary (op, rename_expr re e1, rename_expr re e2)

let rec rename_prop (re : ren) prop =
  match prop with
  | False -> False
  | True -> True
  | Now e -> Now (rename_expr re e)
  | Omega (index, es) -> Omega (index, List.map (rename_expr re) es)
  | Equality (e1, e2) -> Equality (rename_expr re e1, rename_expr re e2)
  | UnaryOp (op, p) -> UnaryOp (op, rename_prop re p)
  | NaryOp (op, ps) -> NaryOp (op, List.map (rename_prop re) ps)
  | BinaryOp (op, p1, p2) ->
    BinaryOp (op, rename_prop re p1, rename_prop re p2)
  | Quantified (q, vars, p) -> Quantified (q, vars, rename_prop re p)

let rec rename_stmt (re : ren) stmt =
  match stmt with
  | Skip -> Skip
  | Assign (x, e) ->
      let x' =
        List.map (fun x -> match VarMap.find_opt x re with
        | Some x' -> x'
        | None -> x) x in
      Assign (x', List.map (rename_expr re) e)
  | Assume p -> Assume (rename_prop re p)
  | Choice (p1, p2) ->
      Choice (
        List.map (rename_stmt re) p1,
        List.map (rename_stmt re) p2)

  | Iterate (specs, body) ->
      Iterate (specs, List.map (rename_stmt re) body)

  (* | While (specs, guard, body) -> *)
  (*     While (specs, rename_expr re guard, List.map (rename_stmt re) body) *)

  (* | If (guard, then_body, else_body) -> *)
  (*     If (rename_expr re guard, *)
  (*         List.map (rename_stmt re) then_body, *)
  (*         List.map (rename_stmt re) else_body) *)

let rename_prog (re : ren) (prog : program) : program =
  List.map (rename_stmt re) prog


let zip (l: 'a list) (l': 'b list) : ('a * 'b) list =
  if (List.length l <> List.length l') then
    failwith "Zip of different length lists"
  else
    let rec zip_aux (l: 'a list) (l': 'b list) : ('a * 'b) list =
      match (l, l') with
      | [], [] -> []
      | a :: r, a' :: r' -> (a, a') :: zip_aux r r'
      | _ -> assert false
    in
    List.rev (zip_aux l l')
    
