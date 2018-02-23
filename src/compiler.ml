open Lang
open Lexer
open Parser

let is_parse = ref false
let is_lex = ref false
let arglist = ref []

let main () =
  (* Recognize flags during parsing *)
  let speclist = [("-parse", Arg.Set is_parse, "prints the stream of tokens.");
    ("-lex", Arg.Set is_lex, "prints the abstract syntax tree")]
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
  else begin
  let exp = Parser.prog Lexer.token lexbuf
  in if is_parse = ref true then Lang.string_of_exp exp.value |> print_endline
  else let value =  Lang.eval exp
  in
     match value with
    | VInt n -> n |> string_of_int |> print_endline
    | VFloat f -> f |> string_of_float |> print_endline
    | VBool b -> b |> string_of_bool |> print_endline end

let _ = if !Sys.interactive then () else main ()
