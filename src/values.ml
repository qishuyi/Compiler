open Expression
open Types

(* Definition of type value *)
type value =
| VInt of int
| VFloat of float
| VBool of bool
| VFun of string * expression
| VFix of string * string * expression
| VUnit
| VPair of value * value
| VEmptylist of t
| VCons of value * value
| VRef of value
| VConstructor of string * (value list)

(* Checks if an expression is a value or not *)
let rec is_value (e:expression) : bool =
match e with
| EInt n             -> true
| EFloat f           -> true
| EBool b            -> true
| EFun (s, e')       -> true
| EFix (f, x, e')    -> true
| EUnit              -> true
| EPair(e1, e2)      -> if (is_value e1.value)&&(is_value e2.value) then true else false
| EEmptylist t       -> true
| ECons(e1, e2)      -> if (is_value e1.value)&&(is_value e2.value) then true else false
| EPointer n         -> true
| EConstructor(s, l) -> true
| _                  -> false

(* Turn the given expression into a value *)
let rec exp_to_value (env:environment)  (e:expression) : value =
let rec exp_list_to_value (env:environment) (l1:exp list) (ret:value list) : value list =
  if List.length l1 = 0 then ret
  else exp_list_to_value env (List.tl l1) (List.append [exp_to_value env (List.hd l1).value] ret)
in
match e with
| EInt n             -> VInt n.value
| EFloat f           -> VFloat f.value
| EBool b            -> VBool b.value
| EFun (s, e')       -> VFun (s.v.v.value, e'.value)
| EFix (f, x, e')    -> VFix (f.value, x.v.v.value, e'.value)
| EUnit              -> VUnit
| EPair(e1, e2)      -> VPair ((exp_to_value env e1.value), (exp_to_value env e2.value))
| EEmptylist t       -> VEmptylist t
| ECons(e1, e2)      -> VCons ((exp_to_value env e1.value), (exp_to_value env e2.value))
| EConstructor(s, l) -> VConstructor(s, (List.rev (exp_list_to_value env l [])))
| _                  -> failwith "Invalid expression; it cannot be evaluated to a value"

(* Take in a value and returns the corresponding string. 
   This function will be used for the purpose of debugging and printing results. *)
let rec string_of_value (v:value) : string =
let rec string_of_constructor_helper (l:value list) (ret:string) : string =
  if List.length l = 1 then (string_of_value (List.hd l)) ^ ret
  else string_of_constructor_helper (List.tl l) ", " ^ (string_of_value (List.hd l)) ^ ret
in
match v with
| VInt n             -> string_of_int n
| VFloat f           -> string_of_float f
| VBool b            -> string_of_bool b
| VFun (s, e')       -> "fun " ^ s ^ " -> " ^ (string_of_exp e')
| VFix (f, x, e')    -> "fix " ^ f ^ " " ^ x ^ " -> " ^ (string_of_exp e')
| VUnit              -> "()"
| VPair(v1, v2)      -> "(" ^ (string_of_value v1) ^ ", " ^ (string_of_value v2) ^ ")"
| VEmptylist t       -> "[] : " ^ (string_of_type t)
| VCons(v1, v2)      -> (string_of_value v1) ^ " :: " ^ (string_of_value v2)
| VRef v             -> "ref " ^ (string_of_value v)
| VConstructor(s, l) -> s ^ "(" ^ (string_of_constructor_helper l "") ^ ")"

