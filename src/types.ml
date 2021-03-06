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
| TVariant of string

(* Turns a type into string *)
let rec string_of_type (typ:t) : string =
(* let rec string_of_type_list (l:t list) (ret:string) : string =
  let type_list = List.rev l in
  if List.length type_list = 0 then ret
  else string_of_type_list (List.tl type_list) (string_of_type (List.hd type_list)) ^ " " ^ ret 
  in*)
  match typ with
  | TInt                -> "int"
  | TFloat              -> "float"
  | TBool               -> "bool"
  | TFun(t1, t2)        -> (string_of_type t1) ^ "->" ^ (string_of_type t2)
  | TUnit               -> "unit"
  | TPair(t1, t2)       -> (string_of_type t1) ^ " * " ^ (string_of_type t2)
  | TList t             -> "[" ^ (string_of_type t) ^ "]"
  | TRef t              -> "<" ^ (string_of_type t) ^ ">"
  | TVariant s          -> s
