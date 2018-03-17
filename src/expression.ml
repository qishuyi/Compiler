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
| EConstructor of string * exp list
| EMatch of exp * pattern_match_suite
and exp = { value : expression ; pos : Lexing.position }
and pattern_match_suite = case list
and case = pattern * exp
and pattern = string * (variable list)

type environment = (int * expression) list

let rec string_of_variable_list (l:variable list) (ret:string) : string =
  let vlist = List.rev l in
  if List.length vlist = 1 then (List.hd vlist).value ^ ret
  else (string_of_variable_list (List.tl vlist) "") ^ ", " ^ (List.hd vlist).value ^ ret

let string_of_pattern (p:pattern) : string =
  (fst p) ^ "(" ^ (string_of_variable_list (snd p) "") ^ ")"

(* Turn the AST into string *)
let rec string_of_exp (e:expression) : string =
  let rec string_of_exp_list (e:exp list) (ret:string) : string =
    if List.length e = 1 then (string_of_exp (List.hd e).value) ^ ret
    else (string_of_exp_list (List.tl e) ", " ^ (string_of_exp (List.hd e).value) ^ ret)
  in
    let string_of_case (c:case) : string =
    "| " ^ (string_of_pattern (fst c)) ^ " -> " ^ (string_of_exp (snd c).value) ^ "\n"
    in
    let rec string_of_pm_suite (pm:pattern_match_suite) (ret:string) : string =
      let pmlist = List.rev pm in
      if List.length pmlist = 0 then ret
      else string_of_pm_suite (List.tl pmlist) (string_of_case (List.hd pmlist)) ^ ret
  in
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
| EApply (e1, e2)                    ->  "(" ^ string_of_exp e1.value ^ " " ^ string_of_exp e2.value ^ ")"
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
| EConstructor (name, elist)        -> name ^ "(" ^ (string_of_exp_list elist "") ^ ")"
  | EMatch(e, pmlist)                 -> "match " ^ (string_of_exp e.value) ^ " with\n" ^ (string_of_pm_suite pmlist "")
