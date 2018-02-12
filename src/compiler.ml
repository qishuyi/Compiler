open Lang
open Lexer
open Parser

let main () =
  let filename = Sys.argv.(1) in
  let tokens   = Lexer.lex (Stream.of_channel (open_in filename)) in
  let (e, _)   = Parser.parse tokens in
  let value = Lang.eval e in
  match value with
  | VInt n -> n|> string_of_int |> print_endline
  | VBool b -> b |> string_of_bool |> print_endline
  | VFloat f -> f |> string_of_float |> print_endline

let _ = if !Sys.interactive then () else main ()
