open Lexing
open List
open Lang

(* A look-up table for referenced expressions *)
type environment = (int * expression) list

(* Returns the line and column numbers of the given position as a string *)
let locate (p:Lexing.position) : string = 
" (" ^ (string_of_int p.pos_lnum) ^ " : " ^ (string_of_int (p.pos_cnum - p.pos_bol + 1)) ^ ")"

(* If variable exists in an expression, substitute it with the given value v; otherwise, do nothing.
 * @param v,        the expression to substitute with
 * @param e,        the expression which v is substituted into
 * @param variable, the name of the variable in e to be substituted by v
 * @returns 
 *)
let substitute (v:exp) (variable:string) (e:exp) : exp =
  let rec subst (e:exp) : exp =
  match e.value with
  | EInt _ | EFloat _ | EBool _ | EUnit | EEmptylist _ | EPointer _ | ERef _  
                                  -> e
    | EVar x                      -> if String.equal x.value variable then v else e
  | EBinop(op, e1, e2)         -> {value=EBinop(op, subst e1, subst e2);pos=e.pos}
    | EIf(e1, e2, e3)             -> {value=EIf(subst e1, subst e2, subst e3);pos=e.pos}
  | ELet(s, typ, e1, e2)             -> {value=ELet(s, typ, subst e1, if String.equal s.value variable then e2 else subst e2);pos=e.pos}
    | EApply(e1, explist)         -> {value=EApply(e1, List.map subst explist);pos=e.pos}
  | EPair(e1, e2)               -> {value=EPair(subst e1, subst e2); pos=e.pos}
    | EFst e'                     -> {value=EFst (subst e'); pos=e.pos}
  | ESnd e'                     -> {value=ESnd (subst e'); pos=e.pos}
    | ECons(e1, e2)               -> {value=ECons(subst e1, subst e2); pos=e.pos}
  | EHead e'                    -> {value=EHead (subst e'); pos=e.pos}
    | ETail e'                    -> {value=ETail (subst e'); pos=e.pos}
  | EEmpty e'                   -> {value=EEmpty (subst e'); pos=e.pos}
    | EUpdate(e1, e2)             -> {value=EUpdate(subst e1, subst e2); pos=e.pos}
  | EDeref e'                   -> {value=EDeref (subst e'); pos=e.pos}
    | EIgnore(e1, e2)             -> {value=EIgnore(subst e1, subst e2); pos = e.pos}
  | EWhile(e1, e2)              -> {value=EWhile(subst e1, subst e2); pos=e.pos}
    | EConstructor (s, explist)   -> let new_list = List.rev (List.map subst explist) in {value=EConstructor (s, new_list); pos=e.pos}
  | EMatch(e', caselist)        -> {value=EMatch(subst e', caselist); pos=e'.pos}
in
subst e

(* In the expression e, substitute each variable in vlist with the corresponding expression in elist at the same index.
 * Then return the values-substituted expression e.
 * If length of variable list is different from that of the expression list, an exception is thrown.
 *)
let rec substitute_multiple (e:exp) (vlist:string list) (elist:exp list) : exp =
 if (List.length vlist = 0) then e
 else substitute_multiple (substitute (List.hd elist) (List.hd vlist) e) (List.tl vlist) (List.tl elist)                 

(* Evaluates an arithmetic expression to the final value.
 * 	
 * @param   op,    the arithmetic operator, consisting of PLUS, MINUS, TIMES, DIVIDE
 * @param   e1 e2, the expressions on both sides of the operator 
 *)
let eval_binop (op:binop) (e1:exp) (e2:exp) : expression = begin 
	match e1.value, op, e2.value with
	(* + *)
	| EInt n1, OAdd, EInt n2                      -> EInt   {value=n1.value + n2.value ; pos=e1.pos}
	| EFloat f1, OAdd, EFloat f2                  -> EFloat {value=f1.value +. f2.value ; pos=e1.pos}
	| EInt n, OAdd, EFloat f                      -> EFloat {value=(float_of_int n.value) +. f.value ; pos=e1.pos}
	| EFloat f, OAdd, EInt n                      -> EFloat {value=f.value +. (float_of_int n.value) ; pos=e1.pos}
	(* - *)
	| EInt n1, OSub, EInt n2                      -> EInt {value=n1.value - n2.value ; pos=e1.pos}
	| EFloat f1, OSub, EFloat f2                  -> EFloat {value=f1.value -. f2.value ; pos=e1.pos}
	| EInt n, OSub, EFloat f                      -> EFloat {value=(float_of_int n.value) -. f.value ; pos=e1.pos}
	| EFloat f, OSub, EInt n                      -> EFloat {value=f.value -. (float_of_int n.value) ; pos=e1.pos}
	(* * *)
	| EInt n1, OMul, EInt n2                      -> EInt {value=n1.value * n2.value ; pos=e1.pos}
	| EFloat f1, OMul, EFloat f2                  -> EFloat {value=f1.value *. f2.value ; pos=e1.pos}
	| EInt n, OMul, EFloat f                      -> EFloat {value=(float_of_int n.value) *. f.value ; pos=e1.pos}
	| EFloat f, OMul, EInt n                      -> EFloat {value=f.value *. (float_of_int n.value) ; pos=e1.pos}
	(* / *)
	| EInt n1, ODiv, EInt n2                      -> if n2.value = 0 then let error_msg = (locate e1.pos) ^ "Division by zero" in failwith error_msg 
	                                                   else EInt {value=n1.value / n2.value ; pos=e1.pos}
	| EFloat f1, ODiv, EFloat f2                  -> if f2.value = 0.0 then let error_msg = (locate e1.pos) ^ "Division by zero" in failwith error_msg
	else EFloat {value=f1.value /. f2.value ; pos=e1.pos}
	| EInt n, ODiv, EFloat f                      -> if f.value = 0.0 then let error_msg = (locate e1.pos) ^ "Division by zero" in failwith error_msg
	                                                   else EFloat {value=(float_of_int n.value) /. f.value ; pos=e1.pos}
	| EFloat f, ODiv, EInt n                      -> if n.value = 0 then let error_msg = (locate e1.pos) ^ "Division by zero" in failwith error_msg
	else EFloat {value=f.value /. (float_of_int n.value) ; pos=e1.pos}
	(* <= *)
	| EInt n1, OLeq, EInt n2                      -> EBool {value=(n1.value <= n2.value) ; pos=e1.pos}
	| EFloat f1, OLeq, EFloat f2                  -> EBool {value=(f1.value <= f2.value) ; pos=e1.pos}
	| EInt n, OLeq, EFloat f                      -> EBool {value=((float_of_int n.value) <= f.value) ; pos=e1.pos}
	| EFloat f, OLeq, EInt n                      -> EBool {value=(f.value <= (float_of_int n.value)) ; pos=e1.pos}
	(* >= *)
	| EInt n1, OGeq, EInt n2                      -> EBool {value=(n1.value >= n2.value) ; pos=e1.pos}
	| EFloat f1, OGeq, EFloat f2                  -> EBool {value=(f1.value >= f2.value) ; pos=e1.pos}
	| EInt n, OGeq, EFloat f                      -> EBool {value=((float_of_int n.value) >= f.value) ; pos=e1.pos}
	| EFloat f, OGeq, EInt n                      -> EBool {value=(f.value >= (float_of_int n.value)) ; pos=e1.pos}
	(* More than *)
	| EInt n1, OGt, EInt n2                       -> EBool {value=(n1.value > n2.value) ; pos=e1.pos}
	| EFloat f1, OGt, EFloat f2                   -> EBool {value=(f1.value > f2.value) ; pos=e1.pos}
	| EInt n, OGt, EFloat f                       -> EBool {value=((float_of_int n.value) > f.value) ; pos=e1.pos}
	| EFloat f, OGt, EInt n                       -> EBool {value=(f.value > (float_of_int n.value)) ; pos=e1.pos}
	(* Less than *)
	| EInt n1, OLt, EInt n2                       -> EBool {value=(n1.value < n2.value) ; pos=e1.pos}
	| EFloat f1, OLt, EFloat f2                   -> EBool {value=(f1.value < f2.value) ; pos=e1.pos}
	| EInt n, OLt, EFloat f                       -> EBool {value=((float_of_int n.value) < f.value) ; pos=e1.pos}
	| EFloat f, OLt, EInt n                       -> EBool {value=(f.value < (float_of_int n.value)) ; pos=e1.pos}
	(* Error messages *)
	| _, _, _ -> let error_msg = (locate e1.pos) ^ "Can only perform arithmetic operations on integer and float expressions" in failwith error_msg end

	let rec step (e:exp) (env:environment) (signatures:variant_signature list * function_signature list) (print_out:bool) : environment * expression =
	let rec step_exp_list (l:exp list) (env:environment) (ret:exp list) : environment * (exp list) = 
	  if List.length l = 0 then (env, ret) else
	  let current = List.hd l in 
      if not (is_value current.value) then let eval = step current env signatures false in step_exp_list (List.append [{value=snd eval; pos=current.pos}] (List.tl l)) (fst eval) ret
	    else step_exp_list (List.tl l) env (List.append [(List.hd l)] ret)
	in
	if print_out then print_endline (string_of_exp e);
	match e.value with
	| EInt n                                      -> (env, EInt n)
	| EFloat f                                    -> (env, EFloat f)
	| EVar x                                      -> let error_msg = (locate e.pos) ^ "Unbound value: " ^ x.value in failwith error_msg
	| EUnit                                       -> (env, EUnit)
  	| EBool b                                     -> (env, EBool b)
	| EBinop (op, e1, e2)                         -> 
	     if not (is_value e1.value) then let eval1 = step e1 env signatures false in (fst eval1, EBinop(op, {value=snd eval1; pos=e1.pos}, e2))
	     else if not (is_value e2.value) then let eval2 = step e2 env signatures false in (fst eval2, EBinop(op, e1, {value=snd eval2;pos=e2.pos}))
	     else begin
	     match e1.value with
	     | EPointer n1 -> begin match e2.value with
	                     | EPointer n2 -> (env, eval_binop op {value=(List.assoc n1 env); pos=e1.pos} {value=(List.assoc n2 env); pos=e2.pos})
	                     | _           -> (env, eval_binop op {value=(List.assoc n1 env); pos=e1.pos} e2) end
	     | _           -> begin match e2.value with
	                     | EPointer n2 -> (env, eval_binop op e1 {value=(List.assoc n2 env); pos=e2.pos})
	                     | _           -> (env, eval_binop op e1 e2) end
	     end
	| EIf (e1, e2, e3)                            ->
	     if not (is_value e1.value) then let eval = step e1 env signatures false in (fst eval, EIf({value=snd eval;pos=e1.pos}, e2, e3))
	     else begin
	     match e1.value with 
	     | EBool b' ->  if b'.value then (step e2 env signatures print_out) else (step e3 env signatures print_out)
	     | _        ->  let error_msg = (locate e1.pos) ^ "This is not a boolean expression." in failwith error_msg end
	| ELet(s, t, e1, e2)                          -> 
	     if not (is_value e1.value) then 
	     	let eval = step e1 env signatures false in (fst eval, ELet(s, t, {value=snd eval;pos=e1.pos}, e2))
	     else let new_exp = (substitute e1 s.value e2) in step new_exp env signatures print_out
	| EApply (func_name, explist)                 -> 
	     let func_info = List.assoc func_name.value (snd signatures) in
         let arglist = func_info.args in
         let func_body = func_info.body in
       if not (List.length explist = List.length arglist) then 
          let error_msg = (locate e.pos) ^ "Function " ^ func_name.value ^ " expects " ^ string_of_int (List.length arglist) ^ " arguments, but here it is applied to " ^ string_of_int (List.length explist) ^ " arguments." in failwith error_msg
       else
          let get_arg_name (a:arg) : string = a.name
            in 
            step (substitute_multiple func_body (List.map get_arg_name arglist) explist) env signatures print_out
    (* List operations *) 
	| EEmptylist t                                -> (env, EEmptylist t)
	| EHead e                                     -> 
	  if not (is_value e.value) then let eval = step e env signatures false in (fst eval, EHead {value=snd eval; pos=e.pos})
	  else begin 
	  match e.value with
	  | EEmptylist t  -> let error_msg = (locate e.pos) ^ "Failure 'hd'" in failwith error_msg
	  | ECons(e1, e2) -> (env, e1.value)
	  | _             -> let error_msg = (locate e.pos) ^ "hd can only operate on lists" in failwith error_msg end
	| ETail e                                     -> 
	  if not (is_value e.value) then let eval = step e env signatures false in (fst eval, ETail {value=snd eval; pos=e.pos})
	  else begin 
	  match e.value with
	  | EEmptylist t  -> let error_msg = (locate e.pos) ^ "Failure 'tl'" in failwith error_msg
	  | ECons(e1, e2) -> (env, e2.value)
	  | _             -> let error_msg = (locate e.pos) ^ "tl can only operate on lists" in failwith error_msg end
	| EEmpty e                                    -> 
	  if not (is_value e.value) then let eval = step e env signatures false in (fst eval, EEmpty {value=snd eval; pos=e.pos})
	  else begin 
	  match e.value with
	  | EEmptylist t  -> (env, EBool {value=true; pos=e.pos})
	  | ECons(e1, e2) -> (env, EBool {value=false; pos=e.pos})
	  | _             -> let error_msg = (locate e.pos) ^ "cons can only operate on lists" in failwith error_msg end
	| ECons(e1, e2)                               ->
	     if not (is_value e1.value) then let eval1 = step e1 env signatures false in (fst eval1, ECons ({value=snd eval1; pos=e.pos}, e2))
	     else if not (is_value e2.value) then let eval2 = step e2 env signatures false in (fst eval2, ECons (e1, {value=snd eval2; pos=e.pos}))
	     else (env, ECons(e1, e2))  
	(* Pair operations *)
	| EPair(e1, e2)                               ->
	     if not (is_value e1.value) then let eval1 = step e1 env signatures false in (fst eval1, EPair({value=snd eval1; pos=e1.pos}, e2))
	     else if not (is_value e2.value) then let eval2 = step e2 env signatures false in (fst eval2, EPair(e1, {value=snd eval2; pos=e2.pos}))
	     else (env, EPair(e1, e2))
	| EFst e                                      ->
	  if not (is_value e.value) then let eval = step e env signatures false in (fst eval, EFst {value=snd eval; pos=e.pos}) else begin 
	  match e.value with
	  | EPair(e1, e2) -> (env, e1.value)
	  | _             -> let error_msg = (locate e.pos) ^ "fst can only operate on pair expressions" in failwith error_msg end
	| ESnd e                                      ->
	  if not (is_value e.value) then let eval = step e env signatures false in (fst eval, ESnd {value=snd eval; pos=e.pos}) else begin
	  match e.value with
	  | EPair(e1, e2) -> (env, e2.value)
	  | _             -> let error_msg = (locate e.pos) ^ "snd can only operate on pair expressions" in failwith error_msg end
	(* Reference operations *)
	| ERef e'                                     -> if not (is_value e'.value) then let eval = step e' env signatures false in (fst eval, ERef({value=snd eval; pos=e'.pos}))
	                                                 else let n = List.length env in ((n, e'.value) :: env, EPointer n) (* We want to treat ref as a value *)
	| EPointer n                                  -> (env, e.value)
	| EUpdate(e1, e2)                             -> if not (is_value e1.value) then let eval1 = step e1 env signatures false in (fst eval1, EUpdate({value=snd eval1; pos=e1.pos}, e2))
	                                                 else if not (is_value e2.value) then let eval2 = step e2 env signatures false in (fst eval2, EUpdate(e1, {value=snd eval2; pos=e2.pos}))
	                                                 else begin
	                                                 match e1.value with
	                                                 | EPointer n -> let new_env = List.remove_assoc n env in ((n, e2.value) :: new_env, EUnit)
							 						 | _                  -> let error_msg = (locate e1.pos) ^ "Can only update value of a reference" in failwith error_msg end
	| EDeref e'                                   -> if not (is_value e'.value) then let eval = step e' env signatures false in (fst eval, EDeref {value=snd eval; pos=e'.pos})
	                                                 else begin
	                                                 match e'.value with
	                                                 | EPointer n     -> (env, List.assoc n env)
	                                                 | _              -> let error_msg = (locate e'.pos) ^ "Nothing to dereference" in failwith error_msg end

	(* While loops and expressions with semicolons *)
	| EIgnore(e1, e2)                             -> if not (is_value e1.value) then let eval1 = step e1 env signatures false in (fst eval1, EIgnore({value=snd eval1; pos=e1.pos}, e2))
	                                                 else if not (is_value e2.value) then let eval2 = step e2 env signatures false in (fst eval2, EIgnore(e1, {value=snd eval2; pos=e2.pos}))
	                                                 else (env, e2.value)
	| EWhile(e1, e2)                              -> let eval1 = ref (step e1 env signatures false) in
	                                                 while not (is_value (snd !eval1)) do eval1 := step {value=snd !eval1; pos=e1.pos} (fst !eval1) signatures false done;
	                                                 begin match (snd !eval1) with
	                                                 | EBool b' -> if b'.value then let eval2 = ref (step e2 (fst !eval1) signatures print_out) in
	                                                               while not (is_value (snd !eval2)) do eval2 := step {value=snd !eval2; pos=e2.pos} (fst !eval2) signatures print_out done ; 
	                                                               ((fst !eval2), EWhile(e1, e2))
	                                                               else (env, EUnit)
	                                                 | _        -> failwith "This is not a boolean expression" end
	(*  *)
	| EConstructor(s, l)                          -> let new_list = step_exp_list l env [] in ((fst new_list), EConstructor(s, (snd new_list)))                   
	| EMatch(e', caselist)                        -> 
	    if not (is_value e'.value) then let eval1 = step e' env signatures false in (fst eval1, EMatch({value=snd eval1; pos=e'.pos}, caselist))
	    else begin 
      match e'.value with
	    | EConstructor(name, explist) -> let pattern_list = fst (List.split caselist) 
                                       in
		                                    let variable_list = List.assoc name.value pattern_list 
                                        in 
		                                      let e1 = List.assoc (name.value, variable_list) caselist 
                                          in
                                          let factor_id (v:variable) : string = v.value
                                          in (env, snd (step (substitute_multiple e1 (List.map factor_id variable_list) explist) env signatures print_out))
		 | _                     -> let error_msg = (locate e'.pos) ^ "We can perform pattern matching on constructor expressions." in failwith error_msg end                      

(* The evaluation function that calls step *)
let eval (e:exp) (env:environment) (signatures:variant_signature list * function_signature list) (b:bool) : value =
  let rec eval_H (e:exp) (envrmt:environment) : value =
    let v = step e envrmt signatures b
    in
    if (is_value (snd v)) then begin print_endline (string_of_exp {value=(snd v);pos=e.pos}) ; (exp_to_value (snd v)) end
    else eval_H {value=(snd v); pos=e.pos} (fst v)
in eval_H e env

