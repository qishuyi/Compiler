open Lang
open Lexer
open Parser

let is_lex = ref false
let is_parse = ref false
let arglist = ref []

let main () =
  let speclist = [("-lex", Arg.Set is_lex, "prints the stream of tokens");
    ("-parse", Arg.Set is_parse, "prints the resulting abstract syntax tree")]in
  let usage_msg = "Usage: ./compiler.ml [flag] [file]" in
    Arg.parse speclist (fun anon -> arglist := anon :: !arglist) usage_msg;

  let filename = Sys.argv.(2) in
  let tokens   = Lexer.lex (Stream.of_channel (open_in filename)) in
  let (e, _)   = Parser.parse tokens in
    if is_lex = ref true then print_endline (Lexer.string_of_token_list tokens)
    else if is_parse = ref true then print_endline (Lang.string_of_exp e)
    else
      let value = Lang.eval e in
      match value with
      | VInt n -> n|> string_of_int |> print_endline
      | VBool b -> b |> string_of_bool |> print_endline
      | VFloat f -> f |> string_of_float |> print_endline

let _ = if !Sys.interactive then () else main ()
