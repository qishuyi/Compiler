type id = string

(* Tokens consist of a value and its position. *)
type int_token = { value : int ; pos : Lexing.position }

type float_token = { value : float ; pos : Lexing.position }

type bool_token = { value : bool ; pos : Lexing.position }

type symbol_token = { value : string ; pos : Lexing.position }

type variable = symbol_token

(* Address is the key to a referenced value in the environment *)
type address = int

(* Definition of type t *)
type t =
| TInt 
| TFloat
| TBool 
| TUnit
| TPair of t * t
| TList of t
| TRef of t
| TVariant of string

type binop = 
| OAdd
| OSub
| OMul
| ODiv
| OLt
| OLeq
| OGt
| OGeq

type pattern = id * variable list

(* Augment AST with position *)
type expression =
| EInt of int_token
| EFloat of float_token
| EBool of bool_token
| EVar of variable
| EBinop of binop * exp * exp
| EIf of exp * exp * exp
| ELet of variable * t * exp * exp
| EApply of variable * exp list
| EUnit
| EPair of exp * exp
| EFst of exp
| ESnd of exp
| EEmptylist of t
| ECons of exp * exp
| EHead of exp
| ETail of exp
| EEmpty of exp
| ERef of exp
| EUpdate of exp * exp
| EDeref of exp
| EPointer of address
| EIgnore of exp * exp
| EWhile of exp * exp
| EConstructor of variable * exp list
| EMatch of exp * case list
and exp = { value : expression ; pos : Lexing.position }
and case = pattern * exp

(*********************************************************************** Functions for values *******************************************************************)
(* Definition of type value *)
type value =
| VInt of int
| VFloat of float
| VBool of bool
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
| EUnit              -> true
| EPair(e1, e2)      -> if (is_value e1.value)&&(is_value e2.value) then true else false
| EEmptylist t       -> true
| ECons(e1, e2)      -> if (is_value e1.value)&&(is_value e2.value) then true else false
| EPointer n         -> true
| EConstructor(s, l) -> true
| _                  -> false

(* Turn a final expression into value *)
let rec exp_to_value (e:expression) : value =
  (* Turns a list of final expressions into a list of values *)
  let rec exp_list_to_value (l1:exp list) (ret:value list) : value list =
  if List.length l1 = 0 then ret
  else exp_list_to_value (List.tl l1) (List.append [exp_to_value (List.hd l1).value] ret)
in
match e with
| EInt n             -> VInt n.value
| EFloat f           -> VFloat f.value
| EBool b            -> VBool b.value
| EUnit              -> VUnit
| EPair(e1, e2)      -> VPair ((exp_to_value e1.value), (exp_to_value e2.value))
| EEmptylist t       -> VEmptylist t
| ECons(e1, e2)      -> VCons ((exp_to_value e1.value), (exp_to_value e2.value))
| EConstructor(s, l) -> VConstructor(s.value, (List.rev (exp_list_to_value l [])))
| _                  -> failwith "Invalid expression; it cannot be evaluated to a value"

(********* function declaration **********)

type arg = { name : id
           ; typ  : t
           }

type fn_nfo = { name    : id
              ; ret_typ : t
              ; args    : arg list
              ; body    : exp
              }

type function_signature = id * fn_nfo

(********** variant declaration ************)

type variant_branch = { tag   : id
                      ; fields  : t list   
                      }

type variant_nfo =  { name  : id
                    ; cases : variant_branch list
                    }

type variant_signature = id * variant_nfo

type signature = 
| VSignature of variant_signature 
| FSignature of function_signature

(*** define general declarations ***)
type declaration = signature list

(******************************************* Pattern-match functions ***************************************************)
let constructor_exists_variant_signature (vs:variant_signature) (target:string) : bool =
  List.exists (fun x -> String.equal x.tag target) (snd vs).cases

let match_pattern (p:pattern) (branchlist:variant_branch list) : bool =
  let rec compare_tag (b:variant_branch) : bool = String.equal (fst p) b.tag
  in
    if List.exists compare_tag branchlist then 
      let branch = List.find compare_tag branchlist
      in
        if not (List.length (snd p) = List.length branch.fields) then 
          let error_msg = "The constructor " ^ (fst p) ^ " expects " ^ (string_of_int (List.length (snd p))) ^ " arguments, but here is applied to " ^ (string_of_int (List.length (snd p))) ^ " arguments." in failwith error_msg
        else true
    else failwith "No matching pattern"

(************************************************************** Pretty printing for types ***************************************************************************)
let rec string_of_type (typ:t) : string =
  match typ with
  | TInt                -> "int"
  | TFloat              -> "float"
  | TBool               -> "bool"
  | TUnit               -> "unit"
  | TPair(t1, t2)       -> (string_of_type t1) ^ " * " ^ (string_of_type t2)
  | TList t             -> "[" ^ (string_of_type t) ^ "]"
  | TRef t              -> "<" ^ (string_of_type t) ^ ">"
  | TVariant s          -> s

(* Writes the list of types into a string, all types separated by space *)
let rec string_of_type_list (l:t list) : string = 
  String.concat " " (List.map string_of_type l)

(***************************************************************** Pretty-printing for expressions *****************************************************************)
let string_of_binop (op:binop) : string = 
match op with
| OAdd -> " + "
| OSub -> " - "
| OMul -> " * "
| ODiv -> " / "
| OLt  -> " < "
| OLeq -> " <= "
| OGt  -> " > "
| OGeq -> " >= "

let string_of_variable (v:variable) : string = v.value

(* Writes a pattern as a string, in the format of "Constructor-name (list-of-variables)" *)
let string_of_pattern (p:pattern) : string =
  (fst p) ^ "(" ^ String.concat ", " (List.map string_of_variable (snd p)) ^ ")"

(* Print out the AST as S-expressions
 * @param e, an expression 
 *)
let rec string_of_exp (e:exp) : string =
  let rec string_of_exp_list (es:exp list) : string = 
  String.concat ", " (List.map string_of_exp es)
  in
  let string_of_case (c:case) : string =
  "| " ^ (string_of_pattern (fst c)) ^ " -> " ^ (string_of_exp (snd c))
  in
  let string_of_case_list (caselist: case list) : string =
  String.concat "\n" (List.map string_of_case caselist)
in
match e.value with
  | EInt n                            ->  string_of_int n.value
| EBool b                           -> string_of_bool b.value
  | EBinop(op, e1, e2)                -> "(" ^ string_of_exp e1 ^ string_of_binop op ^ string_of_exp e2 ^ ")"
| EIf(e1, e2, e3)                   ->  "(if " ^ string_of_exp e1 ^ " "  ^ string_of_exp e2 ^ " " ^ string_of_exp e3 ^ ")"
  | EFloat f                          ->  string_of_float f.value
| EVar x                            ->  x.value
  | ELet (x, t, e1, e2)               ->  "(let " ^ x.value ^ ":" ^ string_of_type t ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2 ^ ")"
| EApply (s, elist)                 ->  "(" ^ s.value ^ " " ^ string_of_exp_list elist ^ ")"
  | EUnit                             -> "()"
| EPair (e1, e2)                    -> "(" ^ string_of_exp e1 ^ ", " ^ string_of_exp e2 ^ ")"
  | EFst e'                           -> "(fst " ^ string_of_exp e' ^ ")"
| ESnd e'                           -> "(snd " ^ string_of_exp e' ^ ")"
  | EEmptylist t                      -> "([] : " ^ string_of_type t ^ ")"
| ECons (e1, e2)                    -> "(" ^ string_of_exp e1 ^ " :: " ^ string_of_exp e2 ^ ")"
  | EHead e'                          -> "(hd " ^ string_of_exp e' ^ ")"
| ETail e'                          -> "(tl " ^ string_of_exp e' ^ ")"
  | EEmpty e'                         -> "(empty " ^ string_of_exp e' ^ ")"
| ERef e'                           -> "(ref " ^ string_of_exp e' ^ ")"
  | EUpdate(e1, e2)                   -> "(" ^ string_of_exp e1 ^ " := " ^ string_of_exp e2 ^ ")"
| EDeref e'                         -> "!" ^ string_of_exp e'
  | EIgnore(e1, e2)                   -> string_of_exp e1 ^ " ; " ^ string_of_exp e2
| EPointer n                        -> "Ptr(" ^ string_of_int n ^ ")"
  | EWhile(e1, e2)                    -> "(while " ^ string_of_exp e1 ^ " do " ^ string_of_exp e2 ^ " end)"
| EConstructor (name, elist)        -> name.value ^ "(" ^ string_of_exp_list elist ^ ")"
  | EMatch(e', caselist)              -> "match " ^ string_of_exp e' ^ " with\n" ^ string_of_case_list caselist

(********************************************************* Pretty-prting for variants **********************************************************)
let rec string_of_variant_branch (branch:variant_branch) : string = 
  if List.length branch.fields = 0 then "| " ^ (branch.tag)
  else "| " ^ (branch.tag) ^ " of " ^ (string_of_type_list branch.fields)

let rec string_of_branch_list (vlist:variant_branch list) =
  String.concat "\n" (List.map string_of_variant_branch vlist)

let string_of_variant_info (info:variant_nfo) : string = "type " ^ (info.name) ^ " =\n" ^ (string_of_branch_list info.cases)

(* Pretty-prniting for function signatures *)
let string_of_arg (a:arg) : string = (string_of_type a.typ) ^ " " ^ a.name

let string_of_arg_list (arglist:arg list) : string =
  String.concat ", " (List.map string_of_arg arglist)

let string_of_fn_info (info:fn_nfo) : string = 
  (string_of_type info.ret_typ) ^ " " ^ info.name ^ "(" ^ (string_of_arg_list info.args) ^ ")\n{\n\t" ^ (string_of_exp info.body) ^ "\n}"

let string_of_signature (signture:signature) : string =
  match signture with
  | VSignature vs  -> string_of_variant_info (snd vs)
  | FSignature fs  -> string_of_fn_info (snd fs)

let string_of_declaration (sigs:signature list) : string =
  String.concat "\n" (List.map string_of_signature sigs)

(**************************************************************Pretty printing for values***********************************************************************)
let rec string_of_value (v:value) : string =
let rec string_of_constructor_helper (l:value list) (ret:string) : string =
  if List.length l = 1 then (string_of_value (List.hd l)) ^ ret
  else string_of_constructor_helper (List.tl l) ", " ^ (string_of_value (List.hd l)) ^ ret
in
match v with
| VInt n             -> string_of_int n
| VFloat f           -> string_of_float f
| VBool b            -> string_of_bool b
| VUnit              -> "()"
| VPair(v1, v2)      -> "(" ^ (string_of_value v1) ^ ", " ^ (string_of_value v2) ^ ")"
| VEmptylist t       -> "[] : " ^ (string_of_type t)
| VCons(v1, v2)      -> (string_of_value v1) ^ " :: " ^ (string_of_value v2)
| VRef v             -> "ref " ^ (string_of_value v)
| VConstructor(s, l) -> s ^ "(" ^ (string_of_constructor_helper l "") ^ ")"
