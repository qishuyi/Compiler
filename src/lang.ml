open Lexing
open List

(* Augment tokens with position *)
type int_token = { value:int ; pos:Lexing.position }

type float_token = { value:float ; pos:Lexing.position }

type bool_token = { value:bool ; pos:Lexing.position }

type symbol_token = { value:string ; pos:Lexing.position }

(* Definition of type t *)
type t =
| TInt 
| TFloat
| TBool 
| TFun of t * t

(* Turns a type into string *)
let rec string_of_type (typ:t) : string =
match typ with
| TInt -> "int"
| TFloat -> "float"
| TBool -> "boolean"
| TFun(t1, t2) -> (string_of_type t1) ^  " -> " ^ (string_of_type t2)

type variable = symbol_token

type ctx = { v : variable ; typ : t }

type fun_ctx = { v : ctx ; typ : t }

type operator = PLUS|MINUS|TIMES|DIVIDE|SMALLEREQUAL|BIGGER

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
and exp = { value : expression ; pos : Lexing.position }

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


(* If variable exists in a function, substitute it with the given value; 
   otherwise, do nothing. *)
let substitute (v:exp) (variable:string) (e:exp) : exp =
   let rec subst (e:exp) : exp =
   match e.value with
   | EInt _ | EFloat _ | EBool _ -> e
   | EVar x                      -> if String.equal x.value variable then v else e
   | EAdd(e1, e2)                -> {value=EAdd(subst e1, subst e2);pos=e.pos}
   | ESubtract(e1, e2)           -> {value=ESubtract(subst e1, subst e2);pos=e.pos}
   | EMultiply(e1, e2)           -> {value=EMultiply(subst e1, subst e2);pos=e.pos}
   | EDivide(e1, e2)             -> {value=EDivide(subst e1, subst e2);pos=e.pos}
   | ELessthanorequal(e1, e2)    -> {value=ELessthanorequal(subst e1, subst e2);pos=e.pos}
   | EIf(e1, e2, e3)             -> {value=EIf(subst e1, subst e2, subst e3);pos=e.pos}
   | EMorethan(e1, e2)           -> {value=EMorethan(subst e1, subst e2);pos=e.pos}
   | ELet(s, e1, e2)             -> {value=ELet(s, subst e1, if String.equal s.v.value variable then e2 else subst e2);pos=e.pos}
   | EFun(s, e')                 -> {value=EFun(s, subst e');pos=e.pos}
   | EFix(f, x, e')              -> {value=EFix(f, x, subst e');pos=e.pos}
   | EApply(e1, e2)              -> {value=EApply(subst e1, subst e2);pos=e.pos}
   in
   subst e

(* Definition of type value *)
type value =
| VInt of int
| VFloat of float
| VBool of bool
| VFun of string * expression
| VFix of string * string * expression

(* Checks if an expression is a value or not *)
let is_value (e:expression) : bool =
match e with
| EInt n -> true
| EFloat f -> true
| EBool b -> true
| EFun (s, e') -> true
| EFix (f, x, e') -> true
| _     -> false

(* Turn the given expression into a value *)
let exp_to_value (e:expression) : value =
match e with
| EInt n -> VInt n.value
| EFloat f -> VFloat f.value
| EBool b -> VBool b.value
| EFun (s, e') -> VFun (s.v.v.value, e'.value)
| EFix (f, x, e') -> VFix (f.value, x.v.v.value, e'.value)
| _  -> failwith "Invalid expression; it cannot be evaluated to a value"

(* Take in a value and returns the corresponding string. 
   This function will be used for the purpose of debugging and printing results. *)
let string_of_value (v:value) : string =
match v with
| VInt n -> string_of_int n
| VFloat f -> string_of_float f
| VBool b -> string_of_bool b
| VFun (s, e') -> "fun " ^ s ^ " -> " ^ (string_of_exp e')
| VFix (f, x, e') -> "fix " ^ f ^ " " ^ x ^ " -> " ^ (string_of_exp e') 

(* Write a position as string *)
let locate (p:Lexing.position) : string = 
" (" ^ (string_of_int p.pos_lnum) ^ " : " ^ (string_of_int (p.pos_cnum - p.pos_bol + 1)) ^ ")"

let print_v (v:variable) : unit =
print_endline v.value

(* Performs typechecking under the given context on the given expression.
   Returns the type of the expression. *)
let rec typecheck (c:(string * t) list) (e:exp) : t = 
  let arith_typecheck (e1:exp) (e2:exp) : t =
    let typ1 = (typecheck c e1) in let typ2 = (typecheck c e2) in begin
    match typ1 with
    | TInt     -> begin match typ2 with
                  | TInt     -> TInt
                  | TFloat   -> TFloat
                  | TBool | TFun _ -> let error_msg = (locate e2.pos) ^ "Expected type: int or float, given type: " ^ (string_of_type typ2) in failwith error_msg end
    | TFloat   -> begin match typ2 with
                  | TInt | TFloat -> TFloat
                  | TBool | TFun _ -> let error_msg = (locate e2.pos) ^ "Expected type: int or float, given type: " ^ (string_of_type typ2) in failwith error_msg end
    | TBool | TFun _ -> let error_msg = (locate e1.pos) ^ "Expected type: int or float, given type: " ^ (string_of_type typ1) in failwith error_msg end
  in
  let compare_typecheck (e1:exp) (e2:exp) : t =
    let typ1 = (typecheck c e1) in let typ2 = (typecheck c e2) in begin
      match typ1 with
      | TInt       -> begin match typ2 with
                      | TInt -> TBool
                      | _ -> let error_msg = (locate e2.pos) ^ "Expected type: int or float, given type: " ^ (string_of_type typ2) in failwith error_msg end
      | TFloat     -> begin match typ2 with
                      | TFloat -> TBool
                      | _      -> let error_msg = (locate e2.pos) ^ "Expected type: int or float, given type: " ^ (string_of_type typ2) in failwith error_msg end
      | _          -> let error_msg = (locate e2.pos) ^ "Expected type: int or float, given type: " ^ (string_of_type typ2) in failwith error_msg end
  in
  match e.value with
  | EInt n                   -> TInt
  | EFloat f                 -> TFloat
  | EBool b                  -> TBool
  | EVar x                   -> if (mem_assoc x.value c) then (List.assoc x.value c) else let error_msg = (locate e.pos) ^ "Unknown variable: " ^ x.value in failwith error_msg
  (* Arithmetic operations on integers *)
  | EAdd(e1, e2)             -> arith_typecheck e1 e2
  | ESubtract(e1, e2)        -> arith_typecheck e1 e2
  | EMultiply(e1, e2)        -> arith_typecheck e1 e2
  | EDivide(e1, e2)          -> arith_typecheck e1 e2
  (* Other expressions *)
  | ELessthanorequal(e1, e2) -> compare_typecheck e1 e2
  | EMorethan(e1, e2)        -> compare_typecheck e1 e2
  | EIf(e1, e2, e3)          -> begin
                             let typ1 = (typecheck c e1) in let typ2 = (typecheck c e2) in let typ3 = (typecheck c e3) in
                             if not (typ1 = TBool) then let error_msg = (locate e1.pos) ^  "The 'if e1 then e2 else e3' statement requires e1 to be of type bool" in failwith error_msg
                             else if not (typ2 = typ3) then let error_msg = (locate e3.pos) ^ "Expected of type: " ^ (string_of_type typ2) ^ ", given type: " ^ (string_of_type typ3) in failwith error_msg
                             else typ2
                             end
  | ELet(s, e1, e2)          -> begin
                             let typ1 = s.typ in let typ2 = (typecheck c e1) in
                             if not (typ1 = typ2) then let error_msg = (locate e1.pos) ^ "Expected of type: " ^ (string_of_type typ1) ^ ", given type: " ^ (string_of_type typ2) in failwith error_msg  
                             else (typecheck (cons (s.v.value, typ1) c) e2)
                             end
  | EFun(x, e)               -> let typ_in = x.v.typ in let typ_ou = x.typ in TFun(typ_in, typ_ou)
  | EFix(f, x, e)            -> let typ_in = x.v.typ in let typ_ou = x.typ in TFun(typ_in, typ_ou)
  | EApply(e1, e2)           -> let typ1 = (typecheck c e1) in
                             match typ1 with
                             | TInt | TFloat | TBool -> print_endline ((string_of_exp e1.value) ^ " " ^ (string_of_type typ1)) ; let error_msg = (locate e1.pos) ^ "This is not a function, it cannot be applied." in failwith error_msg
                             | TFun(t1, t2)          -> begin
                             let typ2 = (typecheck c e2) in
                             if typ2 = t1 then t2 else let error_msg = (locate e2.pos) ^ "Expected of type: " ^ (string_of_type t1) ^ ", given type: " ^ (string_of_type typ2) in failwith error_msg end

(* Write a position as string *)
let locate (p:Lexing.position) : string = " (" ^ (string_of_int p.pos_lnum) ^ " : " ^ (string_of_int (p.pos_cnum - p.pos_bol + 1)) ^ ")"

(* Evaluates an expression to its wrapped-around value *)
let eval_num (e1:exp) (op:operator) (e2:exp) : expression = begin
match e1.value, op, e2.value with
(* Plus *)
| EInt n1, PLUS, EInt n2                        -> EInt   {value=n1.value + n2.value ; pos=e1.pos}
| EFloat f1, PLUS, EFloat f2                    -> EFloat {value=f1.value +. f2.value ; pos=e1.pos}
| EInt n, PLUS, EFloat f                        -> EFloat {value=(float_of_int n.value) +. f.value ; pos=e1.pos}
| EFloat f, PLUS, EInt n                        -> EFloat {value=f.value +. (float_of_int n.value) ; pos=e1.pos}
(* Minus *)
| EInt n1, MINUS, EInt n2                       -> EInt {value=n1.value - n2.value ; pos=e1.pos}
| EFloat f1, MINUS, EFloat f2                   -> EFloat {value=f1.value -. f2.value ; pos=e1.pos}
| EInt n, MINUS, EFloat f                       -> EFloat {value=(float_of_int n.value) -. f.value ; pos=e1.pos}
| EFloat f, MINUS, EInt n                       -> EFloat {value=f.value -. (float_of_int n.value) ; pos=e1.pos}
(* Multiply *)
| EInt n1, TIMES, EInt n2                       -> EInt {value=n1.value * n2.value ; pos=e1.pos}
| EFloat f1, TIMES, EFloat f2                   -> EFloat {value=f1.value *. f2.value ; pos=e1.pos}
| EInt n, TIMES, EFloat f                       -> EFloat {value=(float_of_int n.value) *. f.value ; pos=e1.pos}
| EFloat f, TIMES, EInt n                       -> EFloat {value=f.value *. (float_of_int n.value) ; pos=e1.pos}
(* Divide *)
| EInt n1, DIVIDE, EInt n2                      -> if n2.value = 0 then let error_msg = (locate e1.pos) ^ "Division by zero" in failwith error_msg 
                                                   else EInt {value=n1.value / n2.value ; pos=e1.pos}
| EFloat f1, DIVIDE, EFloat f2                  -> if f2.value = 0.0 then let error_msg = (locate e1.pos) ^ "Division by zero" in failwith error_msg
else EFloat {value=f1.value /. f2.value ; pos=e1.pos}
| EInt n, DIVIDE, EFloat f                      -> if f.value = 0.0 then let error_msg = (locate e1.pos) ^ "Division by zero" in failwith error_msg
                                                   else EFloat {value=(float_of_int n.value) /. f.value ; pos=e1.pos}
| EFloat f, DIVIDE, EInt n                      -> if n.value = 0 then let error_msg = (locate e1.pos) ^ "Division by zero" in failwith error_msg
else EFloat {value=f.value /. (float_of_int n.value) ; pos=e1.pos}
(* Less than or equal *)
| EInt n1, SMALLEREQUAL, EInt n2                -> EBool {value=(n1.value <= n2.value) ; pos=e1.pos}
| EFloat f1, SMALLEREQUAL, EFloat f2            -> EBool {value=(f1.value <= f2.value) ; pos=e1.pos}
| EInt n, SMALLEREQUAL, EFloat f                -> EBool {value=((float_of_int n.value) <= f.value) ; pos=e1.pos}
| EFloat f, SMALLEREQUAL, EInt n                -> EBool {value=(f.value <= (float_of_int n.value)) ; pos=e1.pos}
(* More than *)
| EInt n1, BIGGER, EInt n2                      -> EBool {value=(n1.value > n2.value) ; pos=e1.pos}
| EFloat f1, BIGGER, EFloat f2                  -> EBool {value=(f1.value > f2.value) ; pos=e1.pos}
| EInt n, BIGGER, EFloat f                      -> EBool {value=((float_of_int n.value) > f.value) ; pos=e1.pos}
| EFloat f, BIGGER, EInt n                      -> EBool {value=(f.value > (float_of_int n.value)) ; pos=e1.pos}
(* Error messages *)
| _, (PLUS|MINUS|TIMES|DIVIDE|SMALLEREQUAL|BIGGER), _ -> let error_msg = (locate e1.pos) ^ "Can only perform arithmetic operations on integer and float expressions" in failwith error_msg end

let rec step (e:exp) (b:bool) : expression =
   if b then print_endline (string_of_exp e.value);
   match e.value with
   | EInt n                                      -> EInt n
   | EFloat f                                    -> EFloat f
   | EVar x                                      -> let error_msg = (locate e.pos) ^ "Unbound value: " ^ x.value in failwith error_msg
   | EAdd (e1, e2)                               ->
   if not (is_value e1.value) then EAdd({value=(step e1 false); pos=e1.pos}, e2)
						   else if not (is_value e2.value) then EAdd(e1, {value=(step e2 false);pos=e2.pos})
												 else eval_num e1 PLUS e2
   | ESubtract (e1, e2)                          ->
     if not (is_value e1.value) then ESubtract({value=(step e1 false);pos=e1.pos}, e2)
     else if not (is_value e2.value) then ESubtract(e1, {value=(step e2 false);pos=e2.pos})
     else eval_num e1 MINUS e2
   | EMultiply (e1, e2)                          -> 
   if not (is_value e1.value) then EMultiply({value=(step e1 false);pos=e1.pos}, e2)
						    else if not (is_value e2.value) then EMultiply(e1, {value=(step e2 false);pos=e2.pos})
												 else eval_num e1 TIMES e2
  | EDivide (e1, e2)                            -> 
    if not (is_value e1.value) then EDivide({value=(step e1 false);pos=e1.pos}, e2)
    else if not (is_value e2.value) then EDivide(e1, {value=(step e2 false);pos=e2.pos})
    else eval_num e1 DIVIDE e2
  | EBool b                                     -> EBool b
  | ELessthanorequal (e1, e2)                   -> 
    if not (is_value e1.value) then ELessthanorequal({value=(step e1 false);pos=e1.pos}, e2)
    else if not (is_value e2.value) then ELessthanorequal(e1, {value=(step e2 false);pos=e2.pos})
    else eval_num e1 SMALLEREQUAL e2
  | EMorethan (e1, e2)                          -> if not (is_value e1.value) then EMorethan({value=(step e1 false);pos=e1.pos}, e2)
						   else if not (is_value e2.value) then EMorethan(e1, {value=(step e2 false);pos=e2.pos})
						   else eval_num e1 BIGGER e2
  | EIf (e1, e2, e3)                            -> 
    if not (is_value e1.value) then EIf({value=(step e1 false);pos=e1.pos}, e2, e3)
    else begin
    match e1.value with 
    | EBool b' ->  if b'.value then (step e2 b) else (step e3 b)
    | _        ->  let error_msg = (locate e1.pos) ^ "This is not a boolean expression." in failwith error_msg end
  | ELet(s, e1, e2)           ->
    if not (is_value e1.value) then ELet(s, {value=(step e1 b);pos=e1.pos}, e2) else step (substitute e1 s.v.value e2) b
  | EFun(s, e')                                 -> EFun (s, e')
  | EFix(f, x, e')                              -> EFix (f, x, e')
  | EApply (e1, e2)                             -> 
    if not (is_value e2.value) then EApply (e1, {value=(step e2 false);pos=e2.pos})
    else if not (is_value e1.value) then EApply ({value=(step e1 false);pos=e1.pos}, e2)
    else begin
    match e1.value with
    | EFun(s, e') -> step (substitute e2 s.v.v.value e') b
    | EFix(f, x, e') -> step (substitute e1 f.value (substitute e2 x.v.v.value e')) b
    | _ -> let error_msg = (locate e1.pos) ^ "This is not a function; it cannot be applied." in failwith error_msg end
    

    (* The evaluation function that calls step *)
    let eval (e:exp) (b:bool) : value =
      let rec eval_H (e:exp) : value =
        let v = step e b
        in
        if (is_value v) then begin print_endline (string_of_exp v) ; (exp_to_value v) end
        else eval_H {value=v; pos=e.pos}
      in eval_H e
