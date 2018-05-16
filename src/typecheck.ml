open Lexing
open List
open Evaluation
open Lang

(* Performs typechecking under the given context on the given expression.
 * Returns the type of the expression. Fails with error if the type is not what we expect.
 *
 * @param c, a list of (string * t) pairs that contains variable names and the type of their corresponding values.
 * @param s, a list of variant signatures provided for typechecking constructors
 *)
  let rec typecheck (c:(string * t) list) (signatures:variant_signature list * function_signature list) (e:exp) : t = 
    let rec typecheck_list (elist:exp list) (tlist:t list) : bool =
      if List.length elist = 0 then true
      else
        let etype = typecheck c signatures (List.hd elist) in
          if etype = List.hd tlist then typecheck_list (List.tl elist) (List.tl tlist)
          else let error_msg = "The expression " ^ string_of_exp (List.hd elist) ^ "has type: " ^ string_of_type etype ^ "but is expected of type: " ^ string_of_type (List.hd tlist) ^ "." 
               in failwith error_msg
    in
    (* Types check for arithmetic expressions, including add, subtract, multiply, divide.
     * Makes sure that both expressions have either type int or float.	
     *	
     * @param   e1, e2 are expressions on each side of the arithmetic operator.	
     * @returns On success, the type of the result of the arithmetic operation, either int or float.
     *          Fails with error message otherwise.
     *)	
    let arith_typecheck (e1:exp) (e2:exp) : t =
      let typ1 = (typecheck c signatures e1) in let typ2 = (typecheck c signatures e2) in begin
      match typ1 with
      | TInt     -> begin match typ2 with
                    | TInt     -> TInt
                    | TFloat   -> TFloat
                    | _        -> let error_msg = (locate e2.pos) ^ "Expected type: int or float, given type: " ^ (string_of_type typ2) in failwith error_msg end
      | TFloat   -> begin match typ2 with
                    | TInt     -> TFloat
                    | TFloat   -> TFloat
                    | _        -> let error_msg = (locate e2.pos) ^ "Expected type: int or float, given type: " ^ (string_of_type typ2) in failwith error_msg end
      | _        -> let error_msg = (locate e1.pos) ^ "Expected type: int or float, given type: " ^ (string_of_type typ1) in failwith error_msg end
    in
    (* Types check for comparison expressions, including <=, >.
     * 
     * @param   e1, e2 are expressions on each side of the comparison operator.
     * @returns On success, type bool; otherwise, fails with error.	
     *)  
    let compare_typecheck (e1:exp) (e2:exp) : t =
      let typ1 = (typecheck c signatures e1) in let typ2 = (typecheck c signatures e2) in begin
      match typ1 with
      | TInt       -> begin match typ2 with
                      | TInt -> TBool
                      | _ -> let error_msg = (locate e2.pos) ^ "Expected type: int, given type: " ^ (string_of_type typ2) in failwith error_msg end
      | TFloat     -> begin match typ2 with
                      | TFloat -> TBool
                      | _      -> let error_msg = (locate e2.pos) ^ "Expected type: float, given type: " ^ (string_of_type typ2) in failwith error_msg end
      | _          -> let error_msg = (locate e2.pos) ^ "Expected type: int or float, given type: " ^ (string_of_type typ2) in failwith error_msg end
    in
    let pattern_typecheck (p:pattern * exp) (branches:variant_branch list) : t = 
      let v_branch = List.find (fun x -> String.equal x.tag (fst (fst p))) branches
        in if List.length (snd (fst p)) = List.length v_branch.fields
           then 
             let xlist = List.map (fun (y:variable) : string -> y.value) (snd (fst p)) 
             in
               let string_typ_list = List.combine xlist v_branch.fields
               in
                 typecheck (string_typ_list @ c) signatures (snd p)
           else 
             let error_msg = "The constructor " ^ v_branch.tag ^ " expects " ^ string_of_int (List.length v_branch.fields) ^ " arguments, but here is given " ^ string_of_int (List.length (snd (fst p))) ^ " arguments."
             in failwith error_msg
    in
        match e.value with
        | EInt n                   -> TInt
        | EFloat f                 -> TFloat
        | EBool b                  -> TBool
        | EUnit                    -> TUnit
        | EVar x                   -> if (mem_assoc x.value c) then (List.assoc x.value c) else let error_msg = (locate e.pos) ^ "Unknown variable: " ^ x.value in failwith error_msg
        (* Arithmetic operations on integers *)
        | EBinop(op, e1, e2)       -> begin match op with
                                      | OAdd | OSub | OMul | ODiv   ->  arith_typecheck e1 e2
                                      | OLt  | OLeq | OGt  | OGeq   ->  compare_typecheck e1 e2
                                      end
        | EIf(e1, e2, e3)          -> begin
                                      let typ1 = (typecheck c signatures e1) in let typ2 = (typecheck c signatures e2) in let typ3 = (typecheck c signatures e3) in
                                      if not (typ1 = TBool) then let error_msg = (locate e1.pos) ^  "The 'if e1 then e2 else e3' statement requires e1 to be of type bool" in failwith error_msg
                                      else if not (typ2 = typ3) then let error_msg = (locate e3.pos) ^ "Expected of type: " ^ (string_of_type typ2) ^ ", given type: " ^ (string_of_type typ3) in failwith error_msg
                                      else typ2
                                      end
        | ELet(s, t, e1, e2)          -> begin
                                      let typ = (typecheck c signatures e1) in
                                      if not (t = typ) then let error_msg = (locate e1.pos) ^ s.value ^ "is expected of type: " ^ (string_of_type t) ^ ", given type: " ^ (string_of_type typ) in failwith error_msg  
                                      else (typecheck (cons (s.value, t) c) signatures e2) (* s is a ctx, s.v is a variable/symbol_token, s.v.value is a string *)
                                      end
        | EApply(f, explist)      ->  let function_info = List.assoc f.value (snd signatures) in 
                                        let arg_list = function_info.args in
                                        if not (List.length arg_list = List.length explist) then 
                                          let error_msg = (locate f.pos) ^ "Funtion " ^ f.value ^ " expects " ^ string_of_int (List.length arg_list) ^ " arguments, but here is applied to " ^ string_of_int (List.length explist) ^ " arguments."
                                            in failwith error_msg
                                        else
                                          let rec extract_type_list (alist:arg list) (ret:t list) : t list =
                                              if List.length alist = 0 then ret
                                              else extract_type_list (List.tl alist) ((List.hd alist).typ :: ret)
                                          in
                                            if typecheck_list explist (List.rev (extract_type_list arg_list [])) then function_info.ret_typ else failwith "Syntax error"
        (* List operations *)
        | EEmptylist t             -> TList t
        | EHead e'                 -> let typ = typecheck c signatures e' in begin
                                      match typ with
                                      | TList t -> t
                                      | _       -> let error_msg = (locate e'.pos) ^ "hd can only operate on lists" in failwith error_msg end
        | ETail e'                 -> let typ = typecheck c signatures e' in begin
                                      match typ with
                                      | TList t -> TList t
                                      | _       -> let error_msg = (locate e'.pos) ^ "tl can only operate on lists" in failwith error_msg end  
        | EEmpty e'                -> let typ = typecheck c signatures e' in begin
                                      match typ with
                                      | TList t -> TBool
                                      | _       -> let error_msg = (locate e'.pos) ^ "empty can only operate on lists" in failwith error_msg end 
        | ECons(e1, e2)            -> let typ1 = typecheck c signatures e1 in let typ2 = typecheck c signatures e2 in begin (* Gets type for both e1 and e2 *)
                                      match typ2 with
                                      | TList t -> if (typ1 = t) then typ2 
                                                   else let error_msg = (locate e1.pos) ^ "Expected of type: " ^ (string_of_type t) ^ ", given type: " ^ (string_of_type typ1) in failwith error_msg
                                      | _       -> let error_msg = (locate e2.pos) ^ "Expected of type: list, given type: " ^ (string_of_type typ2) in failwith error_msg end
        (* Operatiosn on pairs *)
        | EPair(e1, e2)            -> let typ1 = typecheck c signatures e1 in let typ2 = typecheck c signatures e2 in TPair(typ1, typ2)
        | EFst e'                  -> let typ = typecheck c signatures e' in begin
                                      match typ with
                                      | TPair(t1, t2) -> t1
                                      | _             -> let error_msg = (locate e'.pos) ^ "Expected of type: pair, given type: " ^ (string_of_type typ) in failwith error_msg end
        | ESnd e'                  -> let typ = typecheck c signatures e' in begin
                                      match typ with
                                      | TPair(t1, t2) -> t2
                                      | _             -> let error_msg = (locate e'.pos) ^ "Expected of type: pair, given type: " ^ (string_of_type typ) in failwith error_msg end
        (* Operation on reference calls *)
        | ERef e'                  -> let typ = typecheck c signatures e' in TRef typ
        | EPointer n               -> let error_msg = "Unknown expression: " ^ (string_of_exp e) in failwith error_msg (* Pointer expressions are not known to users, only for internal use *)
        | EUpdate(e1, e2)          -> let typ1 = typecheck c signatures e1 in let typ2 = typecheck c signatures e2 in begin
                                      match typ1 with
                                      | TRef t -> if (t = typ2) then TUnit
                                                  else let error_msg = (locate e2.pos) ^ "Expected of type: " ^ (string_of_type t) ^ ", given type: " ^ (string_of_type typ2) in failwith error_msg 
                                      | _      -> let error_msg = (locate e1.pos) ^ "Expected of type: ref, given type: " ^ (string_of_type typ1) in failwith error_msg end
        | EDeref e'                -> let typ = typecheck c signatures e' in begin
                                      match typ with
                                      | TRef t -> t
                                      | _      -> let error_msg = (locate e'.pos) ^ "Expected of type: ref, given type: " ^ (string_of_type typ) in failwith error_msg end
        (* While loop and expressions with semicolons *)
        | EIgnore (e1, e2)         -> let typ1 = typecheck c signatures e1 in let typ2 = typecheck c signatures e2 in begin
        								match typ1 with
        								| TUnit -> typ2
        								| _     -> let error_msg = (locate e1.pos) ^ "Expected of type: unit, given type: " ^ (string_of_type typ1) in failwith error_msg end
        | EWhile(e1, e2)           -> let typ1 = typecheck c signatures e1 in let typ2 = typecheck c signatures e2 in begin
                                      match typ1 with
                                      | TBool -> begin match typ2 with
                                                 | TUnit -> TUnit
                                                 | _     -> let error_msg = (locate e2.pos) ^ "Expected of type: unit, given type: " ^ (string_of_type typ2) in failwith error_msg end
                                      | _     -> let error_msg = (locate e1.pos) ^ "Expected of type: bool, given type: " ^ (string_of_type typ1) in failwith error_msg end
        (* Variants and pattern matching *)
        | EConstructor (s, elist)  -> let vsignature = List.find (fun x -> constructor_exists_variant_signature x s.value) (fst signatures) in
                                      let branch = List.find (fun c -> String.equal c.tag s.value) (snd vsignature).cases in  
                                        if List.length elist = List.length branch.fields 
                                        then 
                                          if typecheck_list elist branch.fields then TVariant (fst vsignature) else failwith "syntax error"
                                        else let error_msg = "The constructor " ^ branch.tag ^ " expects " ^ (string_of_int (List.length branch.fields)) ^ " argument(s), but is applied here to " ^ (string_of_int (List.length elist)) ^ " argument(s)." in failwith error_msg 
        | EMatch(e', l)            -> let typ = typecheck c signatures e' in begin
                                      match typ with
                                      | TVariant t -> if List.mem_assoc t (fst signatures) (* Checks if the variant exists in the signatures*)
                                                      then 
                                                        let v_info = List.assoc t (fst signatures) in (* Get the variant's name and branch list *)
                                                          let typlist = List.map (fun x -> pattern_typecheck x v_info.cases) l in
                                                            let rec all_same_type (typ:t) (tlist:t list) : bool =
                                                              if List.length tlist = 0 then true
                                                              else 
                                                                if typ = (List.hd tlist) then all_same_type typ (List.tl tlist)
                                                                else false
                                                            in
                                                              if all_same_type (List.hd typlist) (List.tl typlist) then (List.hd typlist)
                                                              else failwith "Syntax error"
                                                      else 
                                                        let error_msg = "No matchable type: " ^ t in failwith error_msg
                                      | _          -> failwith "Can only perform pattern matching on variant types." 
                                      end
  
  let typechecker (signatures:variant_signature list * function_signature list) (func:function_signature) : t =
    let arguments = (snd func).args in 
      let rec make_list (alist:arg list) (ret:(string * t) list) : (string * t) list = 
        if List.length alist = 0 then ret
        else 
          let cur = List.hd alist in make_list (List.tl alist) ((cur.name, cur.typ) :: ret)
      in  
      let actual_ret_typ = typecheck (make_list arguments []) signatures (snd func).body
        in let expected_ret_typ = (snd func).ret_typ 
           in if actual_ret_typ = expected_ret_typ then actual_ret_typ
              else let error_msg = "The function " ^ (fst func) ^ " has return type: " ^ string_of_type actual_ret_typ ^ ", but is expected of return type: " ^ string_of_type expected_ret_typ ^ "."
                   in failwith error_msg

