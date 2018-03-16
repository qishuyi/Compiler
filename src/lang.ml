open Lexing
open List
open Types
open Expression
open Values
open Signature

type operator = PLUS|MINUS|TIMES|DIVIDE|SMALLEREQUAL|BIGGER

(* Write a position as string *)
let locate (p:Lexing.position) : string = 
" (" ^ (string_of_int p.pos_lnum) ^ " : " ^ (string_of_int (p.pos_cnum - p.pos_bol + 1)) ^ ")"


(* If variable exists in a function, substitute it with the given value; 
   otherwise, do nothing. *)
let substitute (v:exp) (variable:string) (e:exp) : exp =
let rec subst (e:exp) : exp =
  let rec subst_list (l:exp list) (ret:exp list) : exp list =
  if List.length l = 0 then ret
  else subst_list (List.tl l) ((subst (List.hd l)) :: ret)
in
match e.value with
| EInt _ | EFloat _ | EBool _ | EUnit | EEmptylist _ | EPointer _ | ERef _ 
                              -> e
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
| EPair(e1, e2)               -> {value=EPair(subst e1, subst e2); pos=e.pos}
   | EFst e                      -> {value=EFst (subst e); pos=e.pos}
| ESnd e                      -> {value=ESnd (subst e); pos=e.pos}
   | ECons(e1, e2)               -> {value=ECons(subst e1, subst e2); pos=e.pos}
| EHead e                     -> {value=EHead (subst e); pos=e.pos}
   | ETail e                     -> {value=ETail (subst e); pos=e.pos}
| EEmpty e                    -> {value=EEmpty (subst e); pos=e.pos}
   | EUpdate(e1, e2)             -> {value=EUpdate(subst e1, subst e2); pos=e.pos}
| EDeref e                    -> {value=EDeref (subst e); pos=e.pos}
   | EIgnore(e1, e2)             -> {value=EIgnore(subst e1, subst e2); pos = e.pos}
| EWhile(e1, e2)              -> {value=EWhile(subst e1, subst e2); pos=e.pos}
   | EConstructor (s, l)         -> let new_list = List.rev (subst_list l []) in {value=EConstructor (s, new_list); pos=e.pos}
in
subst e

(* Performs typechecking under the given context on the given expression.
   Returns the type of the expression. *)
let rec typecheck (c:(string * t) list) (sigma:signature) (e:exp) : t = 
  let arith_typecheck (e1:exp) (e2:exp) : t =
    let typ1 = (typecheck c e1) in let typ2 = (typecheck c e2) in begin
    match typ1 with
    | TInt     -> begin match typ2 with
                  | TInt     -> TInt
                  | TFloat   -> TFloat
                  | _        -> let error_msg = (locate e2.pos) ^ "Expected type: int or float, given type: " ^ (string_of_type typ2) in failwith error_msg end
    | TFloat   -> begin match typ2 with
                  | TInt     -> TFloat
                  | TFloat        -> TFloat
                  | _        -> let error_msg = (locate e2.pos) ^ "Expected type: int or float, given type: " ^ (string_of_type typ2) in failwith error_msg end
    | _        -> let error_msg = (locate e1.pos) ^ "Expected type: int or float, given type: " ^ (string_of_type typ1) in failwith error_msg end
    in
    let compare_typecheck (e1:exp) (e2:exp) : t =
      let typ1 = (typecheck c e1) in let typ2 = (typecheck c e2) in begin
      match typ1 with
      | TInt       -> begin match typ2 with
                      | TInt -> TBool
                      | _ -> let error_msg = (locate e2.pos) ^ "Expected type: int, given type: " ^ (string_of_type typ2) in failwith error_msg end
      | TFloat     -> begin match typ2 with
                      | TFloat -> TBool
                      | _      -> let error_msg = (locate e2.pos) ^ "Expected type: float, given type: " ^ (string_of_type typ2) in failwith error_msg end
      | _          -> let error_msg = (locate e2.pos) ^ "Expected type: int or float, given type: " ^ (string_of_type typ2) in failwith error_msg end
      in
      match e.value with
      | EInt n                   -> TInt
      | EFloat f                 -> TFloat
      | EBool b                  -> TBool
      | EUnit                    -> TUnit
      | EEmptylist t             -> TList t
      | EHead e                  -> let typ = typecheck c e in begin
                                    match typ with
                                    | TList t -> t
                                    | _       -> let error_msg = (locate e.pos) ^ "hd can only operate on lists" in failwith error_msg end
      | ETail e                  -> let typ = typecheck c e in begin
                                    match typ with
                                    | TList t -> typ
                                    | _       -> let error_msg = (locate e.pos) ^ "tl can only operate on lists" in failwith error_msg end  
      | EEmpty e                 -> let typ = typecheck c e in begin
                                    match typ with
                                    | TList t -> TBool
                                    | _       -> let error_msg = (locate e.pos) ^ "empty can only operate on lists" in failwith error_msg end 
      | ECons(e1, e2)            -> let typ1 = typecheck c e1 in let typ2 = typecheck c e2 in begin
                                    match typ2 with
                                    | TList t -> if (typ1 = t) then typ2 
                                                 else let error_msg = (locate e1.pos) ^ "Expected of type: " ^ (string_of_type t) ^ ", given type: " ^ (string_of_type typ1) in failwith error_msg
                                    | _       -> let error_msg = (locate e2.pos) ^ "Expected of type: list, given type: " ^ (string_of_type typ2) in failwith error_msg end
      | EPair(e1, e2)            -> let typ1 = typecheck c e1 in let typ2 = typecheck c e2 in TPair(typ1, typ2)
      | EFst e                   -> let typ = typecheck c e in begin
                                    match typ with
                                    | TPair(t1, t2) -> t1
                                    | _             -> let error_msg = (locate e.pos) ^ "Expected of type: pair, given type: " ^ (string_of_type typ) in failwith error_msg end
      | ESnd e                   -> let typ = typecheck c e in begin
                                    match typ with
                                    | TPair(t1, t2) -> t2
                                    | _             -> let error_msg = (locate e.pos) ^ "Expected of type: pair, given type: " ^ (string_of_type typ) in failwith error_msg end
      | ERef e                   -> let typ = typecheck c e in TRef typ
      | EPointer n               -> TUnit
      | EUpdate(e1, e2)          -> let typ1 = typecheck c e1 in begin
                                    match typ1 with
                                    | TRef t -> let typ2 = typecheck c e2 in
                                      if (t = typ2) then TUnit
                                      else let error_msg = (locate e2.pos) ^ "Expected of type: " ^ (string_of_type t) ^ ", given type: " ^ (string_of_type typ2) in failwith error_msg 
                                    | _      -> let error_msg = (locate e1.pos) ^ "Expected of type: ref, given type: " ^ (string_of_type typ1) in failwith error_msg end
      | EDeref e                 -> let typ = typecheck c e in begin
                                    match typ with
                                    | TRef t -> t
                                    | _      -> let error_msg = (locate e.pos) ^ "Expected of type: ref, given type: " ^ (string_of_type typ) in failwith error_msg end
      | EIgnore (e1, e2)         -> let typ = typecheck c e2 in typ
      | EWhile(e1, e2)           -> let typ1 = typecheck c e1 in let typ2 = typecheck c e2 in begin
                                    match typ1 with
                                    | TBool -> begin match typ2 with
                                               | TUnit -> TUnit
                                               | _     -> let error_msg = (locate e2.pos) ^ "Expected of type: unit, given type: " ^ (string_of_type typ2) in failwith error_msg end
                                    | _     -> let error_msg = (locate e1.pos) ^ "Expected of type: bool, given type: " ^ (string_of_type typ1) in failwith error_msg end
      | EVar x                   -> if (mem_assoc x.value c) then (List.assoc x.value c) else let error_msg = (locate e.pos) ^ "Unknown variable: " ^ x.value in failwith error_msg
      | EConstructor (s, l)      -> let construct = find_constructor sigma s in
                                    match construct with
                                    | 
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
      | EApply(e1, e2)           -> let typ1 = (typecheck c e1) in begin
                                    match typ1 with
                                    | TFun(t1, t2)             -> begin
                                                                  let typ2 = (typecheck c e2) in
                                                                  if typ2 = t1 then t2
                                                                  else let error_msg = (locate e2.pos) ^ "Expected of type: " ^ (string_of_type t1) ^ ", given type: " ^ (string_of_type typ2)
                                                                  in failwith error_msg end 
                                    | _ -> let error_msg = (locate e1.pos) ^ "Expected of type: function, given type: " ^ (string_of_type typ1) in failwith error_msg end

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

let rec step (e:exp) (env:environment) (sigma:signature) (b:bool) : environment * expression =
if b then print_endline (string_of_exp e.value);
match e.value with
| EInt n                                      -> (env, EInt n)
| EFloat f                                    -> (env, EFloat f)
| EVar x                                      -> let error_msg = (locate e.pos) ^ "Unbound value: " ^ x.value in failwith error_msg
| EUnit                                       -> (env, EUnit)
| EEmptylist t                                -> (env, EEmptylist t)
| EHead e                                     -> 
  if not (is_value e.value) then let eval = step e env sigma false in (fst eval, EHead {value=snd eval; pos=e.pos})
  else begin 
  match e.value with
  | EEmptylist t  -> let error_msg = (locate e.pos) ^ "Failure 'hd'" in failwith error_msg
  | ECons(e1, e2) -> (env, e1.value)
  | _             -> let error_msg = (locate e.pos) ^ "hd can only operate on lists" in failwith error_msg end
| ETail e                                     -> 
  if not (is_value e.value) then let eval = step e env sigma false in (fst eval, ETail {value=snd eval; pos=e.pos})
  else begin 
  match e.value with
  | EEmptylist t  -> let error_msg = (locate e.pos) ^ "Failure 'tl'" in failwith error_msg
  | ECons(e1, e2) -> (env, e2.value)
  | _             -> let error_msg = (locate e.pos) ^ "tl can only operate on lists" in failwith error_msg end
| EEmpty e                                    -> 
  if not (is_value e.value) then let eval = step e env sigma false in (fst eval, EEmpty {value=snd eval; pos=e.pos})
  else begin 
  match e.value with
  | EEmptylist t  -> (env, EBool {value=true; pos=e.pos})
  | ECons(e1, e2) -> (env, EBool {value=false; pos=e.pos})
  | _             -> let error_msg = (locate e.pos) ^ "cons can only operate on lists" in failwith error_msg end
| ECons(e1, e2)                               ->
     if not (is_value e1.value) then let eval1 = step e1 env sigma false in (fst eval1, ECons ({value=snd eval1; pos=e.pos}, e2))
     else if not (is_value e2.value) then let eval2 = step e2 env sigma false in (fst eval2, ECons (e1, {value=snd eval2; pos=e.pos}))
     else (env, ECons(e1, e2))  
| EPair(e1, e2)                               ->
     if not (is_value e1.value) then let eval1 = step e1 env sigma false in (fst eval1, EPair({value=snd eval1; pos=e1.pos}, e2))
     else if not (is_value e2.value) then let eval2 = step e2 env sigma false in (fst eval2, EPair(e1, {value=snd eval2; pos=e2.pos}))
     else (env, EPair(e1, e2))
| EFst e                                      ->
  if not (is_value e.value) then let eval = step e env sigma false in (fst eval, EFst {value=snd eval; pos=e.pos}) else begin 
  match e.value with
  | EPair(e1, e2) -> (env, e1.value)
  | _             -> let error_msg = (locate e.pos) ^ "fst can only operate on pair expressions" in failwith error_msg end
| ESnd e                                      ->
  if not (is_value e.value) then let eval = step e env sigma false in (fst eval, ESnd {value=snd eval; pos=e.pos}) else begin
  match e.value with
  | EPair(e1, e2) -> (env, e2.value)
  | _             -> let error_msg = (locate e.pos) ^ "snd can only operate on pair expressions" in failwith error_msg end
| ERef e                                      -> let eval = ref (step e env sigma false) in
                                                 while not (is_value (snd !eval)) do eval := step {value=snd !eval; pos=e.pos} (fst !eval) sigma false done ; 
                                                 let n = List.length env in ((n, (snd !eval)) :: env, EPointer n) (* We want to treat ref as a value *)
| EPointer n                                  -> (env, e.value)
| EUpdate(e1, e2)                             -> if not (is_value e1.value) then let eval1 = step e1 env sigma false in (fst eval1, EUpdate({value=snd eval1; pos=e1.pos}, e2))
                                                 else if not (is_value e2.value) then let eval2 = step e2 env sigma false in (fst eval2, EUpdate(e1, {value=snd eval2; pos=e2.pos}))
                                                 else begin
                                                 match e1.value with
                                                 | EPointer n -> let new_env = List.remove_assoc n env in ((n, e2.value) :: new_env, EUnit)
						 | _          -> let error_msg = (locate e1.pos) ^ "Can only update value of a reference" in failwith error_msg end
| EDeref e                                    -> if not (is_value e.value) then let eval = step e env sigma false in (fst eval, EDeref {value=snd eval; pos=e.pos})
                                                 else begin
                                                 match e.value with
                                                 | EPointer n     -> (env, List.assoc n env)
                                                 | _              -> let error_msg = (locate e.pos) ^ "Nothing to dereference" in failwith error_msg end

| EIgnore(e1, e2)                             -> if not (is_value e1.value) then let eval1 = step e1 env sigma false in (fst eval1, EIgnore({value=snd eval1; pos=e1.pos}, e2))
                                                 else if not (is_value e2.value) then let eval2 = step e2 env sigma false in (fst eval2, EIgnore(e1, {value=snd eval2; pos=e2.pos}))
                                                 else (env, e2.value)
| EWhile(e1, e2)                              -> let eval1 = ref (step e1 env sigma false) in
                                                 while not (is_value (snd !eval1)) do eval1 := step {value=snd !eval1; pos=e1.pos} (fst !eval1) sigma false done;
                                                 begin match (snd !eval1) with
                                                 | EBool b' -> if b'.value then let eval2 = ref (step e2 (fst !eval1) sigma b) in
                                                               while not (is_value (snd !eval2)) do eval2 := step {value=snd !eval2; pos=e2.pos} (fst !eval2) sigma b done ; ((fst !eval2), EWhile(e1, e2))
                                                               else (env, EUnit)
                                                 | _        -> failwith "This is not a boolean expression" end
| EConstructor(s, l)                          -> begin match sigma with
                                                 | Sig(s2, l2) -> if (branch_list_contains l2 s) then (env, EConstructor(s, l)) else let error_msg = (locate e.pos) ^ "Unbound constructor: " ^ s in failwith error_msg end
| EAdd (e1, e2)                               ->
     if not (is_value e1.value) then let eval1 = step e1 env sigma false in (fst eval1, EAdd({value=snd eval1; pos=e1.pos}, e2))
     else if not (is_value e2.value) then let eval2 = step e2 env sigma false in (fst eval2, EAdd(e1, {value=snd eval2;pos=e2.pos}))
     else begin
     match e1.value with
     | EPointer n1 -> begin match e2.value with
                     | EPointer n2 -> (env, eval_num {value=(List.assoc n1 env); pos=e1.pos} PLUS {value=(List.assoc n2 env); pos=e2.pos})
                     | _           -> (env, eval_num {value=(List.assoc n1 env); pos=e1.pos} PLUS e2) end
     | _           -> begin match e2.value with
                     | EPointer n2 -> (env, eval_num e1 PLUS {value=(List.assoc n2 env); pos=e2.pos})
                     | _           -> (env, eval_num e1 PLUS e2) end
     end
| ESubtract (e1, e2)                          ->
     if not (is_value e1.value) then let eval1 = step e1 env sigma false in (fst eval1, ESubtract({value=snd eval1; pos=e1.pos}, e2))
     else if not (is_value e2.value) then let eval2 = step e2 env sigma false in (fst eval2, ESubtract(e1, {value=snd eval2;pos=e2.pos}))
     else begin
     match e1.value with
     | EPointer n1 -> begin match e2.value with
                     | EPointer n2 -> (env, eval_num {value=(List.assoc n1 env); pos=e1.pos} MINUS {value=(List.assoc n2 env); pos=e2.pos})
                     | _           -> (env, eval_num {value=(List.assoc n1 env); pos=e1.pos} MINUS e2) end
     | _           -> begin match e2.value with
                     | EPointer n2 -> (env, eval_num e1 MINUS {value=(List.assoc n2 env); pos=e2.pos})
                     | _           -> (env, eval_num e1 MINUS e2) end
     end
| EMultiply (e1, e2)                          -> 
     if not (is_value e1.value) then let eval1 = step e1 env sigma false in (fst eval1, EMultiply({value=snd eval1; pos=e1.pos}, e2))
     else if not (is_value e2.value) then let eval2 = step e2 env sigma false in (fst eval2, EMultiply(e1, {value=snd eval2;pos=e2.pos}))
     else  begin
     match e1.value with
     | EPointer n1 -> begin match e2.value with
                     | EPointer n2 -> (env, eval_num {value=(List.assoc n1 env); pos=e1.pos} TIMES {value=(List.assoc n2 env); pos=e2.pos})
                     | _           -> (env, eval_num {value=(List.assoc n1 env); pos=e1.pos} TIMES e2) end
     | _           -> begin match e2.value with
                     | EPointer n2 -> (env, eval_num e1 TIMES {value=(List.assoc n2 env); pos=e2.pos})
                     | _           -> (env, eval_num e1 TIMES e2) end
     end
| EDivide (e1, e2)                            -> 
     if not (is_value e1.value) then let eval1 = step e1 env sigma false in (fst eval1, EDivide({value=snd eval1; pos=e1.pos}, e2))
     else if not (is_value e2.value) then let eval2 = step e2 env sigma false in (fst eval2, EDivide(e1, {value=snd eval2;pos=e2.pos}))
     else begin
     match e1.value with
     | EPointer n1 -> begin match e2.value with
                     | EPointer n2 -> (env, eval_num {value=(List.assoc n1 env); pos=e1.pos} DIVIDE {value=(List.assoc n2 env); pos=e2.pos})
                     | _           -> (env, eval_num {value=(List.assoc n1 env); pos=e1.pos} DIVIDE e2) end
     | _           -> begin match e2.value with
                     | EPointer n2 -> (env, eval_num e1 DIVIDE {value=(List.assoc n2 env); pos=e2.pos})
                     | _           -> (env, eval_num e1 DIVIDE e2) end
     end
| EBool b                                     -> (env, EBool b)
| ELessthanorequal (e1, e2)                   -> 
     if not (is_value e1.value) then let eval1 = step e1 env sigma false in (fst eval1, ELessthanorequal({value=snd eval1; pos=e1.pos}, e2))
     else if not (is_value e2.value) then let eval2 = step e2 env sigma false in (fst eval2, ELessthanorequal(e1, {value=snd eval2;pos=e2.pos}))
     else begin
     match e1.value with
     | EPointer n1 -> begin match e2.value with
                     | EPointer n2 -> (env, eval_num {value=(List.assoc n1 env); pos=e1.pos} SMALLEREQUAL {value=(List.assoc n2 env); pos=e2.pos})
                     | _           -> (env, eval_num {value=(List.assoc n1 env); pos=e1.pos} SMALLEREQUAL e2) end
     | _           -> begin match e2.value with
                     | EPointer n2 -> (env, eval_num e1 SMALLEREQUAL {value=(List.assoc n2 env); pos=e2.pos})
                     | _           -> (env, eval_num e1 SMALLEREQUAL e2) end
     end
| EMorethan (e1, e2)                          ->
     if not (is_value e1.value) then let eval1 = step e1 env sigma false in (fst eval1, EMorethan({value=snd eval1; pos=e1.pos}, e2))
     else if not (is_value e2.value) then let eval2 = step e2 env sigma false in (fst eval2, EMorethan(e1, {value=snd eval2;pos=e2.pos}))
     else begin
     match e1.value with
     | EPointer n1 -> begin match e2.value with
                     | EPointer n2 -> (env, eval_num {value=(List.assoc n1 env); pos=e1.pos} BIGGER {value=(List.assoc n2 env); pos=e2.pos})
                     | _           -> (env, eval_num {value=(List.assoc n1 env); pos=e1.pos} BIGGER e2) end
     | _           -> begin match e2.value with
                     | EPointer n2 -> (env, eval_num e1 BIGGER {value=(List.assoc n2 env); pos=e2.pos})
                     | _           -> (env, eval_num e1 BIGGER e2) end
     end
| EIf (e1, e2, e3)                            ->
     if not (is_value e1.value) then let eval = step e1 env sigma false in (fst eval, EIf({value=snd eval;pos=e1.pos}, e2, e3))
     else begin
     match e1.value with 
     | EBool b' ->  if b'.value then (step e2 env sigma b) else (step e3 env sigma b)
     | _        ->  let error_msg = (locate e1.pos) ^ "This is not a boolean expression." in failwith error_msg end
| ELet(s, e1, e2)                             ->
     if not (is_value e1.value) then let eval = step e1 env sigma false in (fst eval, ELet(s, {value=snd eval;pos=e1.pos}, e2))
     else step (substitute e1 s.v.value e2) env sigma b
| EFun(s, e')                                 -> (env, EFun (s, e'))
| EFix(f, x, e')                              -> (env, EFix (f, x, e'))
| EApply (e1, e2)                             -> 
     if not (is_value e2.value) then let eval2 = step e2 env sigma false in (fst eval2, EApply (e1, {value=snd eval2;pos=e2.pos}))
     else if not (is_value e1.value) then let eval1 = step e1 env sigma false in (fst eval1, EApply ({value=snd eval1;pos=e1.pos}, e2))
     else begin
     match e1.value with
     | EFun(s, e') -> step (substitute e2 s.v.v.value e') env sigma b
     | EFix(f, x, e') -> step (substitute e1 f.value (substitute e2 x.v.v.value e')) env sigma b
     | _ -> let error_msg = (locate e1.pos) ^ "This is not a function; it cannot be applied." in failwith error_msg end

(* The evaluation function that calls step *)
let eval (e:exp) (env:environment) (sigma:signature) (b:bool) : value =
  let rec eval_H (envmt:environment) (signat:signature) (e:exp) : value =
    let v = step e envmt signat b
    in
    if (is_value (snd v)) then begin print_endline (string_of_exp (snd v)) ; (exp_to_value (fst v) (snd v)) end
    else eval_H (fst v) sigma {value=(snd v); pos=e.pos}
    in eval_H env sigma e
