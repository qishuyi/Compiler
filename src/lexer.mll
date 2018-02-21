{
open Lexing
open Parser

exception Lexer_error of string

let symbols : (string * Parser.token) list =
  [ ("(", LPAREN)
  ; (")", RPAREN)
  ; ("+", PLUS)
  ; ("-", MINUS)
  ; ("*", MULTIPLY)
  ; ("/", DIVIDE)
  ; ("<=", SMALLEREQUAL)
  ; ("if", IF)
  ; ("then", THEN)
  ; ("else", ELSE)
  ]

let create_symbol lexbuf =
  let str = lexeme lexbuf in
  List.assoc str symbols

let string_of_token (tok:Parser.token) : string =
    match tok with
    | INT n -> string_of_int n
    | BOOL b -> string_of_bool b
    | FLOAT f -> string_of_float f
    | LPAREN -> "("
    | RPAREN -> ")"
    | PLUS -> "+"
    | MINUS -> "-"
    | MULTIPLY -> "*"
    | DIVIDE -> "/"
    | SMALLEREQUAL -> "<="
    | IF -> "if"
    | THEN -> "then"
    | ELSE -> "else"
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
  | digit+		      { INT (create_int lexbuf) }
  | float+		      { FLOAT (create_float lexbuf) }
  | "true" | "false"	      { BOOL (create_bool lexbuf) }
  | whitespace+ | newline+    { token lexbuf }
  | '(' | ')' | '+' | '-' | '*' | '/' | "<=" | "if" | "then" | "else"
    { create_symbol lexbuf }
  | _ as c { raise @@ Lexer_error("Unexpected character " ^ Char.escaped c) }
