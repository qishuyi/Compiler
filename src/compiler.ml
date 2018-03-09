open Lang
open Lexer
open Parser

let is_parse = ref false
let is_lex = ref false
let is_step = ref false
let arglist = ref []

let main () =
  (* Recognize flags during parsing *)
  let speclist = [("-parse", Arg.Set is_parse, "prints the stream of tokens.");
  ("-lex", Arg.Set is_lex, "prints the abstract syntax tree");
  ("-step", Arg.Set is_step, "show small step evaluation")]
    in let usage_msg = "Usage: my-project [flags] [args]"
  in Arg.parse speclist (fun anon -> arglist :=  anon :: !arglist) usage_msg;

  (* Print out the information corresponding to the flags *)
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
  let exp = Parser.prog Lexer.token lexbuf
  in if is_parse = ref true then Lang.string_of_exp exp.value |> print_endline
  else let value =  Lang.eval exp !is_step
  in
  if !is_step = false then
  Lang.string_of_value value |> print_endline
  
let _ = if !Sys.interactive then () else main ()
