(* Definition of type t *)
type t =
| TInt 
| TFloat
| TBool 
| TFun of t * t
| TUnit
| TPair of t * t
| TList of t
| TRef of t

(* Turns a type into string *)
let rec string_of_type (typ:t) : string =
match typ with
| TInt           -> "int"
| TFloat         -> "float"
| TBool          -> "bool"
| TFun(t1, t2)   -> (string_of_type t1) ^  " -> " ^ (string_of_type t2)
| TUnit          -> "unit"
| TPair(t1, t2)  -> (string_of_type t1) ^ " * " ^ (string_of_type t2)
| TList t        -> "[" ^ (string_of_type t) ^ "]"
| TRef t         -> "<" ^ (string_of_type t) ^ ">"
