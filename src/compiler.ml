open Evaluation
open Lexer
open Parser
open Lang
open Cil

let is_parse = ref false
let is_lex   = ref false
let is_step  = ref false
let is_cil = ref false
let arglist  = ref []

(* Returns a list of variant signatures and a list of function signatures, factored out from the declarations *)
let factor_declaration (dl:Lang.declaration) : Lang.variant_signature list * Lang.function_signature list =
  let rec helper (ds:Lang.declaration) (vlist:Lang.variant_signature list) (flist:Lang.function_signature list) : Lang.variant_signature list * Lang.function_signature list =
    if List.length ds = 0 then (vlist, flist)
    else begin
      match List.hd ds with
      | VSignature v -> helper (List.tl ds) (v :: vlist) flist
      | FSignature f -> helper (List.tl ds) vlist (f :: flist)
    end 
  in helper dl [] []

let main () =
  (* Recognize flags during parsing *)
  let speclist = [("-parse", Arg.Set is_parse, "prints the stream of tokens.");
  ("-lex", Arg.Set is_lex, "prints the abstract syntax tree");
  ("-step", Arg.Set is_step, "show small step evaluation");
  ("-cil", Arg.Set is_cil, "source-to-source translation to a C-like language")]
    in let usage_msg = "Usage: my-project [flags] [args]"
  in Arg.parse speclist (fun anon -> arglist :=  anon :: !arglist) usage_msg;

  (* If the -lex flag is specified, print out the tokens *)
  let lexbuf = Sys.argv.(1) |> open_in |> Lexing.from_channel
  in
  if is_lex = ref true then begin
  let rec append_list (toks : Parser.token list) (buf : Lexing.lexbuf) =
    let tok = Lexer.token buf in
    match tok with
    | EOF   ->    toks
    | _     ->    append_list (tok :: toks) buf
  in
  let toks = append_list [] lexbuf in
  "[" ^ (Lexer.string_of_token_list (List.rev toks)) ^ "]" |> print_endline end
  else
    let decllist = Parser.prog Lexer.token lexbuf 
    in
    let signatures = factor_declaration decllist
    in
    (* If the -parse flag is specified, print out the AST *)
    if is_parse = ref true then (string_of_declaration decllist) |> print_endline
    else 
      let type_check (f:Lang.function_signature) : Lang.t = Typecheck.typechecker signatures f
      in 
        if is_cil = ref true then 
        begin
          List.map type_check (snd signatures) |> ignore;
          Cil.conv_fns (List.rev (snd signatures)) [] signatures |> Cil.string_of_fns |> print_endline
        end
        else
        let function_info = List.assoc "main" (snd signatures) in
          Evaluation.eval function_info.body [] signatures !is_step |> string_of_value |> print_endline 
      
  
let _ = if !Sys.interactive then () else main ()
