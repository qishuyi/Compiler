type exp =
| EInt of int
| EAdd of exp * exp
| ESubtract of exp * exp
| EMultiply of exp * exp
| EDivide of exp * exp
| EBool of bool
| ELessthanorequal of exp * exp
| EIf of exp * exp * exp
| EFloat of float

type value =
| VInt of int
| VBool of bool
| VFloat of float

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

let rec eval (e:exp) : value =
  match e with
  | EInt n                    -> VInt n
  | EFloat f                  -> VFloat f
  | EAdd (e1, e2)             -> begin
    match eval e1 with 
    | VInt n -> begin
      match eval e2 with
      | VInt n -> VInt (int_interpret (eval e1) + int_interpret (eval e2))
      | VFloat f -> VFloat (float_of_int (int_interpret (eval e1)) +. float_interpret (eval e2))
      | VBool b -> failwith "Cannot perform arithmetic operations on boolean expressions" end
    | VFloat f -> begin
      match eval e2 with
      | VInt n -> VFloat (float_interpret (eval e1) +. float_of_int (int_interpret (eval e2)))
      | VFloat f -> VFloat (float_interpret (eval e1) +. float_interpret (eval e2))
      | VBool b -> failwith "Cannot perform arithmetic operations on boolean expressions" end
    | VBool b -> failwith "Cannot perform arithmetic operations on boolean expressions" end
  | ESubtract (e1, e2)        -> begin
    match eval e1 with 
    | VInt n -> begin
      match eval e2 with
      | VInt n -> VInt (int_interpret (eval e1) - int_interpret (eval e2))
      | VFloat f -> VFloat (float_of_int (int_interpret (eval e1)) -. float_interpret (eval e2))
      | VBool b -> failwith "Cannot perform arithmetic operations on boolean expressions" end
    | VFloat f -> begin
      match eval e2 with
      | VInt n -> VFloat (float_interpret (eval e1) -. float_of_int (int_interpret (eval e2)))
      | VFloat f -> VFloat (float_interpret (eval e1) -. float_interpret (eval e2))
      | VBool b -> failwith "Cannot perform arithmetic operations on boolean expressions" end
    | VBool b -> failwith "Cannot perform arithmetic operations on boolean expressions" end
  | EMultiply (e1, e2)        -> begin
    match eval e1 with 
    | VInt n -> begin
      match eval e2 with
      | VInt n -> VInt (int_interpret (eval e1) * int_interpret (eval e2))
      | VFloat f -> VFloat (float_of_int (int_interpret (eval e1)) *. float_interpret (eval e2))
      | VBool b -> failwith "Cannot perform arithmetic operations on boolean expressions" end
    | VFloat f -> begin
      match eval e2 with
      | VInt n -> VFloat (float_interpret (eval e1) *. float_of_int (int_interpret (eval e2)))
      | VFloat f -> VFloat (float_interpret (eval e1) *. float_interpret (eval e2))
      | VBool b -> failwith "Cannot perform arithmetic operations on boolean expressions" end
    | VBool b -> failwith "Cannot perform arithmetic operations on boolean expressions" end
  | EDivide (e1, e2)          -> begin
    match eval e1 with 
    | VInt n -> begin
      match eval e2 with
      | VInt n -> 
         if int_interpret (eval e2) = 0 then failwith "Division by zero" 
         else VInt (int_interpret (eval e1) / int_interpret (eval e2))
      | VFloat f ->
         if int_of_float (float_interpret (eval e2)) = 0 then failwith "Division by zero" 
         else VFloat (float_of_int (int_interpret (eval e1)) /. float_interpret (eval e2))
      | VBool b -> failwith "Cannot perform arithmetic operations on boolean expressions" end
    | VFloat f -> begin
      match eval e2 with
      | VInt n -> 
         if int_interpret (eval e2) = 0 then failwith "Division by zero" 
         else VFloat (float_interpret (eval e1) /. float_of_int (int_interpret (eval e2)))
      | VFloat f ->
         if int_of_float (float_interpret (eval e2)) = 0 then failwith "Division by zero" 
         else VFloat (float_interpret (eval e1) +. float_interpret (eval e2))
      | VBool b -> failwith "Cannot perform arithmetic operations on boolean expressions" end
    | VBool b -> failwith "Cannot perform arithmetic operations on boolean expressions" end
  | EBool b                   -> VBool b
  | ELessthanorequal (e1, e2) -> begin
      match eval e1 with
      | VInt n -> begin
         match eval e2 with
         | VInt n -> VBool (int_interpret (eval e1) <= int_interpret (eval e2))
         | VFloat f -> VBool (float_of_int (int_interpret (eval e1)) <= float_interpret (eval e2))
         | VBool b -> failwith "Cannot perform arithmetic operations on boolean expressions" end
      |VFloat f -> begin
        match eval e2 with
        | VInt n -> VBool (float_interpret (eval e1) <= float_of_int (int_interpret (eval e2)))
        | VFloat f -> VBool (float_interpret (eval e1) <= float_interpret (eval e2))
        | VBool b -> failwith "Cannot perform arithmetic operations on boolean expressions" end
      |VBool b -> failwith "Cannot perform arithmetic operations on boolean expressions" end
  | EIf (e1, e2, e3)          ->
    let if_check (e2:exp) (e3:exp) : bool = 
      match eval e2 with
      | VInt n -> begin 
         match eval e3 with 
         | VInt n   -> true
         | VBool b  -> false
         | VFloat f -> false end      
      | VBool b -> begin
         match eval e3 with
         | VBool b  -> true
         | VInt n   -> false
         | VFloat f -> false end
      | VFloat f -> begin
         match eval e3 with
         | VFloat f -> true
         | VInt n   -> false
         | VBool b  -> false end
     in
     if (if_check e2 e3) then begin 
       if bool_interpret (eval e1) then eval e2 else eval e3 end
     else failwith "The syntax 'if a1 then a2 else a3' requires a2 and a3 to be of the same type"
