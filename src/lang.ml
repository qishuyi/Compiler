open Lexing

(* Augment tokens with position *)
type int_token = { value : int ; pos : Lexing.position }

type float_token = { value : float ; pos : Lexing.position }

type bool_token = { value : bool ; pos : Lexing.position }

type symbol_token = { value : string ; pos : Lexing.position }

(* Augment AST with position *)
type expression =
| EInt of int_token
| EAdd of exp * exp
| ESubtract of exp * exp
| EMultiply of exp * exp
| EDivide of exp * exp
| EBool of bool_token
| ELessthanorequal of exp * exp
| EIf of exp * exp * exp
| EFloat of float_token
and exp = {value:expression; pos:Lexing.position}

(* Wrap around the return value of eval *)
type value =
| VInt of int
| VBool of bool
| VFloat of float

(* Turn the AST into string *)
let rec string_of_exp (e:expression) : string =
  match e with
  | EInt n                            ->  string_of_int(n.value)
  | EAdd(e1, e2)                      ->
      "(+ " ^ string_of_exp e1.value ^ " " ^ string_of_exp e2.value ^ ")"
  | ESubtract(e1, e2)                 ->
      "(- " ^  string_of_exp e1.value ^ " " ^ string_of_exp e2.value ^ ")"
  | EMultiply(e1, e2)                 ->
      "(* " ^  string_of_exp e1.value ^ " " ^ string_of_exp e2.value ^ ")"
  | EDivide(e1, e2)                   ->
      "(/ " ^  string_of_exp e1.value ^ " " ^ string_of_exp e2.value ^ ")"
  | EBool b                           ->  string_of_bool(b.value)
  | ELessthanorequal(e1, e2)          ->
      "(<= " ^ string_of_exp e1.value ^ " " ^ string_of_exp e2.value ^ ")"
  | EIf(e1, e2, e3)                   ->
       "(if " ^ string_of_exp e1.value ^ " "  ^ string_of_exp e2.value ^ " " ^ string_of_exp e3.value ^ ")"
  | EFloat f                          ->  string_of_float(f.value)

(* Write a position as string *)
let locate (p:Lexing.position) : string = 
" (" ^ (string_of_int p.pos_lnum) ^ " : " ^ (string_of_int (p.pos_cnum - p.pos_bol + 1)) ^ ")"

(* Helper functions for eval that deal with
 * Integer values
 * Float values
 * Boolean values
 *)
let int_interpret (v:value) : int = 
  match v with
  | VInt n -> n
  | _      -> failwith "Expected an integer expression."

let bool_interpret (v:value) : bool =
  match v with
  | VBool b -> b
  | _       -> failwith "Expected a boolean expression."

let float_interpret (v:value) : float =
  match v with
  | VFloat f -> f
  | _        -> failwith "Expected a float expression."

(* Evaluates an expression to its wrapped-around value *)
let rec eval (e:exp) : value =
  match e.value with
  | EInt n                    -> VInt n.value
  | EFloat f                  -> VFloat f.value
  | EAdd (e1, e2)             -> begin
    match eval e1 with 
    | VInt n -> begin
      match eval e2 with
      | VInt n   -> VInt (int_interpret (eval e1) + int_interpret (eval e2))
      | VFloat f -> VFloat (float_of_int (int_interpret (eval e1)) +. float_interpret (eval e2))
      | VBool b -> let error_msg = (locate e2.pos) ^ "Cannot perform arithmetic operations on boolean expressions" in failwith error_msg end
    | VFloat f -> begin
      match eval e2 with
      | VInt n -> VFloat (float_interpret (eval e1) +. float_of_int (int_interpret (eval e2)))
      | VFloat f -> VFloat (float_interpret (eval e1) +. float_interpret (eval e2))
      | VBool b -> let error_msg =  (locate e2.pos) ^ "Cannot perform arithmetic operations on boolean expressions" in failwith error_msg end
    | VBool b ->  let error_msg =  (locate e1.pos) ^ "Cannot perform arithmetic operations on boolean expressions" in failwith error_msg end
  | ESubtract (e1, e2)        -> begin
    match eval e1 with 
    | VInt n -> begin
      match eval e2 with
      | VInt n -> VInt (int_interpret (eval e1) - int_interpret (eval e2))
      | VFloat f -> VFloat (float_of_int (int_interpret (eval e1)) -. float_interpret (eval e2))
      | VBool b -> let error_msg = (locate e2.pos) ^ "Cannot perform arithmetic operations on boolean expressions" in failwith error_msg end
    | VFloat f -> begin
      match eval e2 with
      | VInt n -> VFloat (float_interpret (eval e1) -. float_of_int (int_interpret (eval e2)))
      | VFloat f -> VFloat (float_interpret (eval e1) -. float_interpret (eval e2))
      | VBool b -> let error_msg = (locate e2.pos) ^ "Cannot perform arithmetic operations on boolean expressions" in failwith error_msg end
    | VBool b -> let error_msg = (locate e1.pos) ^ "Cannot perform arithmetic operations on boolean expressions" in failwith error_msg end
  | EMultiply (e1, e2)        -> begin
    match eval e1 with 
    | VInt n -> begin
      match eval e2 with
      | VInt n -> VInt (int_interpret (eval e1) * int_interpret (eval e2))
      | VFloat f -> VFloat (float_of_int (int_interpret (eval e1)) *. float_interpret (eval e2))
      | VBool b -> let error_msg = (locate e2.pos) ^ "Cannot perform arithmetic operations on boolean expressions" in failwith error_msg end
    | VFloat f -> begin
      match eval e2 with
      | VInt n -> VFloat (float_interpret (eval e1) *. float_of_int (int_interpret (eval e2)))
      | VFloat f -> VFloat (float_interpret (eval e1) *. float_interpret (eval e2))
      | VBool b -> let error_msg =  (locate e2.pos) ^ "Cannot perform arithmetic operations on boolean expressions" in failwith error_msg end
    | VBool b -> let error_msg = (locate e1.pos) ^ "Cannot perform arithmetic operations on boolean expressions" in failwith error_msg end
  | EDivide (e1, e2)          -> begin
    match eval e1 with 
    | VInt n -> begin
      match eval e2 with
      | VInt n -> 
         if int_interpret (eval e2) = 0
         then let error_msg = (locate e2.pos) ^ "Division by zero" in failwith error_msg
         else VInt (int_interpret (eval e1) / int_interpret (eval e2))
      | VFloat f ->
         if int_of_float (float_interpret (eval e2)) = 0 
         then let error_msg = (locate e2.pos) ^ "Division by zero" in failwith error_msg
         else VFloat (float_of_int (int_interpret (eval e1)) /. float_interpret (eval e2))
      | VBool b -> let error_msg = (locate e1.pos) ^ "Cannot perform arithmetic operations on boolean expressions" in failwith error_msg end
    | VFloat f -> begin
      match eval e2 with
      | VInt n -> 
         if int_interpret (eval e2) = 0
         then let error_msg = (locate e2.pos) ^ "Division by zero" in failwith error_msg
         else VFloat (float_interpret (eval e1) /. float_of_int (int_interpret (eval e2)))
      | VFloat f ->
         if int_of_float (float_interpret (eval e2)) = 0
         then let error_msg = (locate e2.pos) ^ "Division by zero" in failwith error_msg
         else VFloat (float_interpret (eval e1) +. float_interpret (eval e2))
      | VBool b ->  let error_msg = (locate e2.pos) ^ "Cannot perform arithmetic operations on boolean expressions" in failwith error_msg end
    | VBool b ->  let error_msg = (locate e1.pos) ^ "Cannot perform arithmetic operations on boolean expressions" in failwith error_msg end
  | EBool b                   -> VBool b.value
  | ELessthanorequal (e1, e2) -> begin
      match eval e1 with
      | VInt n -> begin
         match eval e2 with
         | VInt n -> VBool (int_interpret (eval e1) <= int_interpret (eval e2))
         | VFloat f -> VBool (float_of_int (int_interpret (eval e1)) <= float_interpret (eval e2))
         | VBool b ->  let error_msg = (locate e2.pos) ^ "Cannot perform arithmetic operations on boolean expressions" in failwith error_msg end
      |VFloat f -> begin
        match eval e2 with
        | VInt n -> VBool (float_interpret (eval e1) <= float_of_int (int_interpret (eval e2)))
        | VFloat f -> VBool (float_interpret (eval e1) <= float_interpret (eval e2))
        | VBool b ->  let error_msg = (locate e2.pos) ^ "Cannot perform arithmetic operations on boolean expressions" in failwith error_msg end
      |VBool b ->  let error_msg = (locate e1.pos) ^ "Cannot perform arithmetic operations on boolean expressions" in failwith error_msg end
  | EIf (e1, e2, e3)          ->
    let then_else_check (e2:exp) (e3:exp) : bool = 
      match eval e2 with
      | VInt n -> begin 
         match eval e3 with 
         | VInt n   -> true
         | _        -> false end      
      | VBool b -> begin
         match eval e3 with
         | VBool b  -> true
         | _        -> false end
      | VFloat f -> begin
         match eval e3 with
         | VFloat f -> true
         | _        -> false end
      in 
      match eval e1 with
      | VBool b    ->   begin
      if (then_else_check e2 e3) then begin 
        if bool_interpret (eval e1) then eval e2 else eval e3 end
      else let error_msg = (locate e2.pos) ^ "The syntax 'if a1 then a2 else a3' requires a2 and a3 to be of the same type" in failwith error_msg end
      | _          ->   let error_msg = (locate e1.pos) ^ "The syntax 'if a1 then a2 else a3' requires e1 to be a boolean expression" in failwith error_msg
