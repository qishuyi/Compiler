open Types

type int_token = { value:int ; pos:Lexing.position }

type float_token = { value:float ; pos:Lexing.position }

type bool_token = { value:bool ; pos:Lexing.position }

type symbol_token = { value:string ; pos:Lexing.position }

type address = int

type variable = symbol_token

type ctx = { v : variable ; typ : t }

type fun_ctx = { v : ctx ; typ : t }

(* Augment AST with position *)
type expression =
| EInt of int_token
| EFloat of float_token
| EBool of bool_token
| EVar of variable
(* Arithmetic operations on integers *)
| EAdd of exp * exp
| ESubtract of exp * exp
| EMultiply of exp * exp
| EDivide of exp * exp
(* Other expressions *)
| ELessthanorequal of exp * exp
| EMorethan of exp * exp
| EIf of exp * exp * exp
| ELet of ctx * exp * exp
| EFun of fun_ctx * exp
| EFix of variable * fun_ctx * exp
| EApply of exp * exp
| EUnit
| EPair of exp * exp
| EFst of exp
| ESnd of exp
| EEmptylist of t
| ECons of exp * exp
| EHead of exp
| ETail of exp
| EEmpty of exp
| ERef of exp
| EUpdate of exp * exp
| EDeref of exp
| EPointer of address
| EIgnore of exp * exp
| EWhile of exp * exp
and exp = { value : expression ; pos : Lexing.position }

type environment = (int * expression) list

(* Turn the AST into string *)
let rec string_of_exp (e:expression) : string =
match e with
| EInt n                            ->  string_of_int(n.value)
  | EAdd(e1, e2)                      ->  "(+ " ^ string_of_exp e1.value ^ " " ^ string_of_exp e2.value ^ ")"
| ESubtract(e1, e2)                 ->  "(- " ^  string_of_exp e1.value ^ " " ^ string_of_exp e2.value ^ ")"
  | EMultiply(e1, e2)                 ->  "(* " ^  string_of_exp e1.value ^ " " ^ string_of_exp e2.value ^ ")"
| EDivide(e1, e2)                   ->  "(/ " ^  string_of_exp e1.value ^ " " ^ string_of_exp e2.value ^ ")"
  | EBool b                           ->  string_of_bool(b.value)
| ELessthanorequal(e1, e2)          ->  "(<= " ^ string_of_exp e1.value ^ " " ^ string_of_exp e2.value ^ ")"
  | EMorethan(e1, e2)                 ->  "(> " ^ string_of_exp e1.value ^ " " ^ string_of_exp e2.value ^ ")"
| EIf(e1, e2, e3)                   ->  "(if " ^ string_of_exp e1.value ^ " "  ^ string_of_exp e2.value ^ " " ^ string_of_exp e3.value ^ ")"
  | EFloat f                          ->  string_of_float(f.value)
| EVar x                            ->  x.value
  | ELet (x, e1, e2)                  ->  "(let " ^ x.v.value ^ " = " ^ string_of_exp e1.value ^ " in " ^ string_of_exp e2.value ^ ")"
| EFun (x, e')                      ->  "(fun " ^ x.v.v.value ^ " -> " ^ string_of_exp e'.value ^ ")"
  | EFix (f, x, e')                   ->  "(fix " ^ f.value ^ " " ^ x.v.v.value ^ " -> " ^ string_of_exp e'.value ^ ")"
| EApply (e1, e2)                   ->  "(" ^ string_of_exp e1.value ^ " " ^ string_of_exp e2.value ^ ")"
  | EUnit                             -> "()"
| EPair (e1, e2)                    -> "(" ^ string_of_exp e1.value ^ ", " ^ string_of_exp e2.value ^ ")"
  | EFst e                            -> "(fst " ^ string_of_exp e.value ^ ")"
| ESnd e                            -> "(snd " ^ string_of_exp e.value ^ ")"
  | EEmptylist t                      -> "([] : " ^ (string_of_type t) ^ ")"
| ECons (e1, e2)                    -> "(" ^ (string_of_exp e1.value) ^ " :: " ^ (string_of_exp e2.value) ^ ")"
  | EHead e                           -> "(hd " ^ (string_of_exp e.value) ^ ")"
| ETail e                           -> "(tl " ^ (string_of_exp e.value) ^ ")"
  | EEmpty e                          -> "(empty " ^ (string_of_exp e.value) ^ ")"
| ERef e                            -> "(ref " ^ (string_of_exp e.value) ^ ")"
  | EUpdate(e1, e2)                   -> "(" ^ (string_of_exp e1.value) ^ " := " ^ (string_of_exp e2.value) ^ ")"
| EDeref e                          -> "!" ^ (string_of_exp e.value)
  | EIgnore(e1, e2)                   -> (string_of_exp e1.value) ^ " ; " ^ (string_of_exp e2.value)
| EPointer n                        -> "Ptr(" ^ (string_of_int n) ^ ")"
  | EWhile(e1, e2)                    -> "(while " ^ (string_of_exp e1.value) ^ " do " ^ (string_of_exp e2.value) ^ " end)"

