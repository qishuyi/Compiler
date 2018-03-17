open Types

type branch = string * (t list)

type signature = string * (branch list)

let rec string_of_type_list (l:t list) (ret:string) : string = 
  let type_list = List.rev l in
  if List.length type_list = 0 then ret
  else string_of_type_list (List.tl type_list) (string_of_type (List.hd type_list)) ^ " " ^ ret

let compare_constructor (b:branch) (target:string) : bool = if String.equal (fst b) target then true else false

let rec branch_list_contains (l:branch list) (target:string) : bool = 
  if List.length l = 0 then false
  else 
    if (compare_constructor (List.hd l) target) then true 
    else (branch_list_contains (List.tl l) target)

let rec signature_contains (s:signature) (target:string) : bool =
  if branch_list_contains (snd s) target then true else false

let rec find_constructor_in_branch_list (l:branch list) (target:string) : branch =
  if List.length l = 0 then let error_msg = "Unknown constructor: " ^ target in failwith error_msg
  else 
    if (compare_constructor (List.hd l) target) then (List.hd l)
    else (find_constructor_in_branch_list (List.tl l) target)

let find_constructor_in_signature (s:signature) (target:string) : branch = find_constructor_in_branch_list (snd s) target

let rec find_constructor_in_signature_list (sl:signature list) (target:string) : signature =
  if List.length sl = 0 then let error_msg = "Unknown constructor: " ^ target in failwith error_msg
  else if signature_contains (List.hd sl) target then (List.hd sl)
       else find_constructor_in_signature_list (List.tl sl) target

let rec string_of_branch (b:branch) : string = "| " ^ (fst b) ^ " of " ^ (string_of_type_list (snd b) "")

let rec string_of_branch_list (l:branch list) (ret:string) : string =
  let branch_list = List.rev l in
  if List.length l = 0 then ret
  else string_of_branch_list (List.tl branch_list) (string_of_branch (List.hd branch_list)) ^ ret

let string_of_signature (s:signature) : string = "type " ^ (fst s) ^ " =\n" ^ (string_of_branch_list (snd s) "")

let rec string_of_signature_list (sl:signature list) (ret:string) : string =
  if List.length sl = 0 then ret
  else string_of_signature_list (List.tl sl) (string_of_signature (List.hd sl)) ^ ret
