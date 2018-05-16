open Lang
open Typecheck

(* short-hand for the Lang module *)
module S = Lang

type id = string

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

type pattern = id * id list

(* Augment AST with position *)
type exp =
| EInt of int
| EFloat of float
| EBool of bool
| EVar of id
| EBinop of binop * exp * exp
| EIf of exp * exp * exp
| ELet of id * t * exp * exp
| EApply of id * exp list
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
| EConstructor of id * exp list
| EMatch of exp * case list
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

type stmt =
| SInit of id * t * exp
| SDecl of id * t
| SAssign of id * exp
| SWhile of exp * exp
| SIf of exp * stmt list * stmt list
| SRet of exp
| SPrint of exp
| SStruct of id * (id * t) list

(********* function declaration **********)

type arg = { name : id
           ; typ  : t
           }

type fn_nfo = { name    : id
              ; ret_typ : t
              ; args    : arg list
              ; body    : stmt list
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

let string_of_binop (op:binop) : string =
  match op with
  | OAdd -> "+"
  | OSub -> "-"
  | OMul -> "*"
  | ODiv -> "/"
  | OLt  -> "<"
  | OLeq -> "<="
  | OGt  -> ">"
  | OGeq -> ">="

let rec sep_str_of_list (sep:string) (ss:string list) : string =
  match ss with
  | [] -> ""
  | [s] -> s
  | s :: ss -> Printf.sprintf "%s %s %s" s sep (sep_str_of_list sep ss)

let rec string_of_exp (e:exp) : string =
  match e with
  | EInt n  -> string_of_int n
  | EBool b -> string_of_bool b
  | EFloat f -> string_of_float f
  | EVar x  -> x
  | EBinop (op, e1, e2) ->
      Printf.sprintf "(%s %s %s)"
        (string_of_exp e1) (string_of_binop op) (string_of_exp e2)
  | EIf (e1, e2, e3) ->
      Printf.sprintf "(%s ? %s : %s)"
        (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
  | EApply (f, es) ->
      Printf.sprintf "%s(%s)"
        f (List.map string_of_exp es |> sep_str_of_list ",")
  | EPair (e1, e2) ->
  	  Printf.sprintf "{.fst = %s, .snd = %s}"
  	  	(string_of_exp e1) (string_of_exp e2)
  | EFst e' ->
  	  Printf.sprintf "%s.fst" (string_of_exp e')
  | ESnd e' ->
  	  Printf.sprintf "%s.snd" (string_of_exp e')

let rec string_of_typ (typ:t) : string =
  match typ with
  | TInt  -> "int"
  | TBool -> "bool"
  | TFloat -> "float"
  | TUnit -> "void"
  | TPair(t1, t2) -> 
  		Printf.sprintf "struct pair_%s_%s" (string_of_typ t1) (string_of_typ t2)
  | TList t' -> 
  		Printf.sprintf "List<%s>" (string_of_typ t')
  | TRef t' -> 
  		Printf.sprintf "%s*" (string_of_typ t')
  | TVariant s -> s

let rec string_of_stmt (s:stmt) : string =
  match s with
  | SInit (x, t, e) ->
      Printf.sprintf "%s %s = %s;" (string_of_typ t) x (string_of_exp e)
  | SAssign (x, e) ->
      Printf.sprintf "%s = %s;" x (string_of_exp e)
  | SDecl (x, t) ->
  	  Printf.sprintf "%s %s;" (string_of_typ t) x
  | SWhile (e1, e2) ->
  	  Printf.sprintf "while (%s) { %s }"
  	  	(string_of_exp e1) (string_of_exp e2)
  | SIf (e, b1, b2) ->
      Printf.sprintf "if (%s) { %s } else { %s }"
        (string_of_exp e) (string_of_blk b1) (string_of_blk b2)
  | SRet e ->
      Printf.sprintf "return %s;" (string_of_exp e)
  | SPrint e ->
      Printf.sprintf "printf(\"%%d\\n\", %s);" (string_of_exp e)
  | SStruct (x, l) ->
  	  Printf.sprintf "struct %s { %s };" x (String.concat " " (List.map (fun (s, t) -> Printf.sprintf "%s %s;" (string_of_typ t) s) l))

and string_of_blk (ss:stmt list) : string =
  List.fold_left
    (fun ret s -> Printf.sprintf "%s %s" ret (string_of_stmt s))
    "" ss

let string_of_arg (a:arg) : string =
  Printf.sprintf "%s %s" (string_of_typ a.typ) a.name

let string_of_fn (f:fn_nfo) : string =
  Printf.sprintf "%s %s(%s) { %s }\n"
    (string_of_typ f.ret_typ) f.name
    (List.map string_of_arg f.args |> sep_str_of_list ",")
    (string_of_blk f.body)

let string_of_fns (p:function_signature list) : string =
  "#include <stdio.h>\n#include <stdbool.h>\n" ^ 
  begin
    List.fold_left
      (fun ret (_, f) -> Printf.sprintf "%s %s" ret (string_of_fn f))
      "" p
  end

(************************************************************** Functions *********************************************************************)

let rec conv_binop (op:S.binop) : binop =
 match op with
 | S.OAdd -> OAdd
 | S.OSub -> OSub
 | S.OMul -> OMul
 | S.ODiv -> ODiv
 | S.OLt  -> OLt
 | S.OLeq -> OLeq
 | S.OGt  -> OGt
 | S.OGeq -> OGeq

let rec conv_typ (typ:S.t) : t =
 match typ with
 | S.TInt 			-> TInt
 | S.TFloat 		-> TFloat
 | S.TBool 			-> TBool 	
 | S.TUnit			-> TUnit
 | S.TPair(t1, t2)	-> TPair(conv_typ t1, conv_typ t2)
 | S.TList t'		-> TList (conv_typ t')
 | S.TRef t'		-> TRef (conv_typ t')
 | S.TVariant s 	-> TVariant s

let fresh_counter = ref 0 ;;

let rec fresh_name () : string =
  let n = !fresh_counter in
  fresh_counter := !fresh_counter + 1;
  "_x" ^ string_of_int n

let rec conv_exp (e:S.exp) (env:(string * S.t) list) (signatures:S.variant_signature list * S.function_signature list): exp * stmt list =
  match e.value with
  | S.EUnit -> (EUnit, [])
  | S.EInt n -> (EInt n.value, [])
  | S.EBool b -> (EBool b.value, [])
  | S.EFloat f -> (EFloat f.value, [])
  | S.EVar x -> (EVar x.value, [])
  | S.EBinop (op, e1, e2) ->
      let (x1, ss1, s1) = factor_subexp e1 env signatures in
      let (x2, ss2, s2) = factor_subexp e2 env signatures in
      (EBinop (conv_binop op, EVar x1, EVar x2), ss1 @ ss2 @ [s1; s2])
  | S.EIf (e1, e2, e3) -> 
      let (x1, ss1, s1) = factor_subexp e1 env signatures in
      let (x2, ss2, s2) = factor_subexp e2 env signatures in
      let (x3, ss3, s3) = factor_subexp e3 env signatures in
      let ret           = fresh_name () in
      let typ 			= conv_typ (Typecheck.typecheck env signatures e2) in
      let ss2'          = ss2 @ [s2] @ [SAssign (ret, EVar x2)] in
      let ss3'          = ss3 @ [s3] @ [SAssign (ret, EVar x3)] in
      (EVar ret, ss1 @ [s1] @ [SDecl (ret, typ)] @ [SIf (EVar x1, ss2', ss3')])
  | S.ELet (x, t, e1, e2) ->
      let (e1', ss1)    = conv_exp e1 env signatures in
      let (x1, ss, s1)  = factor_subexp e1 [] signatures in
      let (x2, ss2, s2) = factor_subexp e2 [(x.value, t)] signatures in
      let typ 			= conv_typ (Typecheck.typecheck env signatures e1) in
      (EVar x2, ss1 @ [SInit (x1, typ, e1')] @ ss2 @ [s2])
  | S.EApply (f, es) -> 
      let (es, ss) = factor_subexps es env signatures in (EApply (f.value, es), ss)
  | S.EIgnore (e1, e2) ->
      let (x1, ss1, s1) = factor_subexp e1 env signatures in
      let (x2, ss2, s2) = factor_subexp e2 env signatures in
      (EVar x2, ss1 @ ss2 @ [s1; s2])
  | S.EPair (e1, e2) -> 
  	  let (x1, ss1, s1) = factor_subexp e1 env signatures in
  	  let (x2, ss2, s2) = factor_subexp e2 env signatures in
  	  let typ1 			= conv_typ (Typecheck.typecheck env signatures e1) in
  	  let typ2 			= conv_typ (Typecheck.typecheck env signatures e2) in
  	  let ret 			= fresh_name() in 
  	  let pair_name 	= "pair_" ^ (string_of_typ typ1) ^ "_" ^ (string_of_typ typ2) in
  	  let pair_fields	= [("fst", typ1)] @ [("snd", typ2)] in
  	  (EVar ret, [SStruct (pair_name, pair_fields)] @ ss1 @ ss2 @ [s1; s2] @ [SInit (ret, TPair(typ1, typ1), EPair(EVar x1, EVar x2))])
  | S.EFst e' -> 
  	  let (x, ss, s)	= factor_subexp e' env signatures in
  	  let (e'', ss')	= conv_exp e' env signatures in
  	  let ret 			= fresh_name() in
  	  let typ 			= Typecheck.typecheck env signatures e' in
  	  begin
  	  	match typ with
  	  	| TPair (t1, t2) -> (EVar ret, ss @ [s] @ [SInit (ret, conv_typ t1, EFst (EVar x))])
  	  	| _              -> failwith "Syntax error"
  	  end
  | S.ESnd e' -> failwith "conv_exp: unimplemented (S.ESnd)"
  | S.EEmptylist t -> failwith "conv_exp: unimplemented (S.EEmptylist)"
  | S.ECons(e1, e2) -> failwith "conv_exp: unimplemented (S.ECons)"
  | S.EHead e' -> failwith "conv_exp: unimplemented (S.EHead)"
  | S.ETail e' -> failwith "conv_exp: unimplemented (S.ETail)"
  | S.EEmpty e' -> failwith "conv_exp: unimplemented (S.EEmpty)"
  | S.ERef _    -> failwith "conv_exp: unimplemented (S.ERef)"
  | S.EDeref _  -> failwith "conv_exp: unimplemented (S.EDeref)"
  | S.EUpdate _ -> failwith "conv_exp: unimplemented (S.EAssign)"
  | S.EPointer _ -> failwith "conv_exp: unimplemented (S.EPointer)"
  | S.EWhile _  -> failwith "conv_exp: unimplemented (S.EWhile)"

and factor_subexp (e:S.exp) (env:(string * S.t) list) (signatures:S.variant_signature list * S.function_signature list) : id * stmt list * stmt =
  let x = ref (fresh_name ()) in
  let (e', ss) = conv_exp e env signatures in
  let typ = Typecheck.typecheck env signatures e in
  let s = SInit (!x, conv_typ typ, e') in
  (!x, ss, s)

and factor_subexps (es:S.exp list) (env:(string * S.t) list) (signatures:S.variant_signature list * S.function_signature list) : exp list * stmt list =
  List.fold_left (fun (es, ss) e ->
    let (x, ss', s) = factor_subexp e env signatures in
    (es @ [EVar x], ss @ ss' @ [s])) ([], []) es

let conv_arg (a:S.arg) : arg =
  { name = a.name
  ; typ  = conv_typ a.typ
  }

let conv_fn (f:S.fn_nfo) (env:(string * S.t) list) (signatures:S.variant_signature list * S.function_signature list) : fn_nfo =
  let args = f.args in
  let str_typ_list = List.map (fun (a:S.arg) -> (a.name, a.typ)) args in
  let (e, ss) = conv_exp f.body (str_typ_list @ env) signatures in
  { name    = f.name
  ; ret_typ = conv_typ f.ret_typ
  ; args    = List.map conv_arg f.args
  ; body    = ss @ [ if f.name = "main" then SPrint e else SRet e ]
  }

let conv_fns (p:S.function_signature list) (env:(string * S.t) list) (signatures:S.variant_signature list * S.function_signature list) : function_signature list =
  List.map (fun (n, f) -> (n, conv_fn f env signatures)) p
