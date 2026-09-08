
(** {2 Abstract Syntax of an Imp language} *)

type location = Lexing.position * Lexing.position

type ident = { loc: location; id: string; }

(* unary operators for expressions *)
type uope = Usucc 

(* binary operators for expressions *)
type bope = Badd | Bsub | Bmul | Bdiv | Bmod | Beq | Ble | Blt

(* constants *)
type cst = Czero | Cone

type expr =
  | Ecst of cst
  | Evar of ident
  | Eunop of uope * expr
  | Ebinop of bope * expr * expr

(* unary operators for props *)
type uopp = Unot | Ux | Ug | Uf 

(* binary operators for props *)
type bopp = Bimp | Bu | Bw | Bxw | Br | Bxr 

(* n-ary operators for props *)
type nopp = Nand | Nor

(* quantifiers for props *)
type quant = Qall | Qex 

type spec = Swf | Smu | Snu

(* props *)
type prop =
  | Pfalse | Ptrue
  | Punop of uopp * prop
  | Pnop of uopp * prop list
  | Pbop of bopp * prop * prop

type stmt =
  | Sskip
  | Sassign of ident * expr
  | Sassume of prop 
  | Schoice of stmt list * stmt list 
  | Siter of spec list * stmt list 
  | Swhile of spec list * expr * stmt list
  | Sif of expr * stmt list * stmt list

type file = stmt
