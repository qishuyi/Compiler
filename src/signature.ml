open Types

type branch = 
| SConstructor of string
| Constructor of string * t list

type signature =
| Sig of string * branch list

let compare_constructor (b:branch) (target:string) : bool =
  match b with
  | SConstructor s      -> if (String.equal s target) then true else false
  | Constructor (s2, l) -> if (String.equal s2 target) then true else false

let rec branch_list_contains (l:branch list) (target:string) : bool = 
  if List.length l = 0 then false
  else 
    if (compare_constructor (List.hd l) target) then true 
    else (branch_list_contains (List.tl l) target)

let rec find_constructor_helper (l:branch list) (target:string) : branch =
  if List.length l = 0 then failwith "Not_found"
  else 
    if (compare_constructor (List.hd l) target) then (List.hd l)
    else (find_constructor_helper (List.tl l) target)

let find_constructor (s:signature) (target:string) : branch = 
  match s with
  | Sig (name, branch_list) -> find_constructor_helper branch_list target

let rec string_of_type_list (l:t list) (ret:string) : string =
  let type_list = List.rev l in
  if List.length l = 0 then ret
  else string_of_type_list (List.tl l) (string_of_type (List.hd l)) ^ " " ^ ret

let rec string_of_branch (b:branch) : string =
  match b with
  | SConstructor s     -> "| " ^ s ^ "\n"
  | Constructor (s, l) -> "| " ^ s ^ " of " ^ (string_of_type_list l "") ^ "\n"

let rec string_of_branch_list (l:branch list) (ret:string) : string =
  let branch_list = List.rev l in
  if List.length l = 0 then ret
  else string_of_branch_list (List.tl branch_list) (string_of_branch (List.hd branch_list)) ^ ret

let string_of_signature (s:signature) : string =
    match s with
    | Sig (s, l) -> "type " ^ s ^ " =\n" ^ (string_of_branch_list l "")

