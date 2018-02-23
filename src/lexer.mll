{
open Lexing
open Parser
open Lang

exception Lexer_error of string

let string_of_token (tok:Parser.token) : string =
    match tok with
    | INT n -> string_of_int n.value ^ (locate n.pos)
    | BOOL b -> string_of_bool b.value ^ (locate b.pos)
    | FLOAT f -> string_of_float f.value ^ (locate f.pos)
    | LPAREN s -> "'('" ^ (locate s.pos)
    | RPAREN s -> "')'" ^ (locate s.pos)
    | PLUS s -> "+" ^ (locate s.pos)
    | MINUS s -> "-" ^ (locate s.pos)
    | MULTIPLY s -> "*" ^ (locate s.pos)
    | DIVIDE s -> "/" ^ (locate s.pos)
    | SMALLEREQUAL s -> "<=" ^ (locate s.pos)
    | IF s -> "'if'" ^ (locate s.pos)
    | THEN s -> "'then'" ^ (locate s.pos)
    | ELSE s -> "'else'" ^ (locate s.pos)
    | EOF -> ""

let string_of_token_list (toks:Parser.token list) : string =
    String.concat "," (List.map string_of_token toks)

let create_int lexbuf = lexeme lexbuf |> int_of_string
let create_float lexbuf = lexeme lexbuf |> float_of_string
let create_bool lexbuf = lexeme lexbuf |> bool_of_string
}

let newline    = '\n' | ('\r' '\n') | '\r'
let whitespace = ['\t' ' ']
let digit      = ['0'-'9']
let float      = digit '.' digit* | "nan"

rule token = parse
  | eof                       { EOF }
  | digit+
    { INT ({value = (create_int lexbuf); pos = lexbuf.Lexing.lex_start_p}) }
  | float+
    { FLOAT ({value = (create_float lexbuf); pos = lexbuf.Lexing.lex_start_p}) }
  | "true" | "false"
    { BOOL ({value = (create_bool lexbuf); pos = lexbuf.Lexing.lex_start_p}) }
  | whitespace+	     	       { token lexbuf }
  | newline+   		       { Lexing.new_line lexbuf; token lexbuf }
  | "("		{LPAREN        ({value="(" ; pos=lexbuf.Lexing.lex_start_p}) }
  | ")"		{RPAREN        ({value=")" ; pos=lexbuf.Lexing.lex_start_p}) }
  | "+"		{PLUS          ({value="+" ; pos=lexbuf.Lexing.lex_start_p}) }
  | "-"		{MINUS         ({value="-" ; pos=lexbuf.Lexing.lex_start_p}) }
  | "*"		{MULTIPLY      ({value="*" ; pos=lexbuf.Lexing.lex_start_p}) }
  | "/"		{DIVIDE        ({value="/" ; pos=lexbuf.Lexing.lex_start_p}) }
  | "<="        {SMALLEREQUAL  ({value="(" ; pos=lexbuf.Lexing.lex_start_p}) }
  | "if"	{IF 	       ({value="if" ; pos=lexbuf.Lexing.lex_start_p}) }
  | "then"	{THEN 	       ({value="then" ; pos=lexbuf.Lexing.lex_start_p}) }
  | "else"	{ELSE 	       ({value="else" ; pos=lexbuf.Lexing.lex_start_p}) }
  | _ as c { raise @@ Lexer_error("Unexpected character " ^ Char.escaped c) }
