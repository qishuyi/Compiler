{
open Lexing
open Parser
open Lang

exception Lexer_error of string

let string_of_token (tok:Parser.token) : string =
match tok with
| INT n			-> string_of_int n.value ^ (locate n.pos)
| BOOL b 	   -> string_of_bool b.value ^ (locate b.pos)
| FLOAT f 	   -> string_of_float f.value ^ (locate f.pos)
| VAR x 	   -> x.value ^ (locate x.pos)
| LPAREN s 	   -> "'('" ^ (locate s.pos)
| RPAREN s 	   -> "')'" ^ (locate s.pos)
| PLUS s 	   -> "+" ^ (locate s.pos)
| MINUS s 	   -> "-" ^ (locate s.pos)
| MULTIPLY s 	   -> "*" ^ (locate s.pos)
| DIVIDE s 	   -> "/" ^ (locate s.pos)
| SMALLEREQUAL s   -> "<=" ^ (locate s.pos)
| GREATER s    	   -> ">" ^ (locate s.pos)
| IF s 	 	   -> "'if'" ^ (locate s.pos)
| THEN s 	   -> "'then'" ^ (locate s.pos)
| ELSE s 	   -> "'else'" ^ (locate s.pos)
| LET s 	   -> "'let'" ^ (locate s.pos)
| BE s 		   -> "=" ^ (locate s.pos)
| IN s 		   -> "'in'" ^ (locate s.pos)
| FUN s 	   -> "'fun'" ^ (locate s.pos)
| OUTPUT s 	   -> "->" ^ (locate s.pos)
| FIX s  	   -> "fix" ^ (locate s.pos)
| EOF 		   -> ""

let string_of_token_list (toks:Parser.token list) : string =
String.concat "," (List.map string_of_token toks)

let create_int lexbuf = lexeme lexbuf |> int_of_string
let create_float lexbuf = lexeme lexbuf |> float_of_string
let create_bool lexbuf = lexeme lexbuf |> bool_of_string
let create_variable lexbuf = lexeme lexbuf
}

let newline    = '\n' | ('\r' '\n') | '\r'
let whitespace = ['\t' ' ']
let digit      = ['0'-'9']
let float      = digit '.' digit* | "nan"
let variable   = ['A'-'z'] | ['0'-'9']

rule token = parse
| eof                       { EOF }
| digit+		    { INT   ({value = (create_int lexbuf); pos = lexbuf.Lexing.lex_start_p}) }
| float+		    { FLOAT ({value = (create_float lexbuf); pos = lexbuf.Lexing.lex_start_p}) }
| "true" | "false"	    { BOOL  ({value = (create_bool lexbuf); pos = lexbuf.Lexing.lex_start_p}) }
| whitespace+	     	    { token lexbuf }
| newline+   		    { Lexing.new_line lexbuf; token lexbuf }
| "("			    {LPAREN        ({value="(" ; pos=lexbuf.Lexing.lex_start_p}) }
| ")"			    {RPAREN        ({value=")" ; pos=lexbuf.Lexing.lex_start_p}) }
| "+"			    {PLUS          ({value="+" ; pos=lexbuf.Lexing.lex_start_p}) }
| "-"			    {MINUS         ({value="-" ; pos=lexbuf.Lexing.lex_start_p}) }
| "*"			    {MULTIPLY      ({value="*" ; pos=lexbuf.Lexing.lex_start_p}) }
| "/"			    {DIVIDE        ({value="/" ; pos=lexbuf.Lexing.lex_start_p}) }
| "<="        		    {SMALLEREQUAL  ({value="<="; pos=lexbuf.Lexing.lex_start_p}) }
| ">"			    {GREATER       ({value=">" ; pos=lexbuf.Lexing.lex_start_p})}
| "="			    {BE	       	   ({value="=" ; pos=lexbuf.Lexing.lex_start_p}) }
| "->"			    {OUTPUT	   ({value="->"; pos=lexbuf.Lexing.lex_start_p})}
| "if"			    {IF 	   ({value="if" ; pos=lexbuf.Lexing.lex_start_p}) }
| "then"		    {THEN 	   ({value="then" ; pos=lexbuf.Lexing.lex_start_p}) }
| "else"		    {ELSE 	   ({value="else" ; pos=lexbuf.Lexing.lex_start_p}) }
| "let"       		    {LET	   ({value="let" ; pos=lexbuf.Lexing.lex_start_p})}
| "in"			    {IN	      	   ({value="in" ; pos=lexbuf.Lexing.lex_start_p})}
| "fun"			    {FUN	   ({value="fun" ; pos=lexbuf.Lexing.lex_start_p})}
| "fix"			    {FIX	   ({value="fix" ; pos=lexbuf.Lexing.lex_start_p})}
| variable+		    {VAR  	   ({value=create_variable lexbuf ; pos=lexbuf.Lexing.lex_start_p})}
| _ as c 		    { raise @@ Lexer_error("Unexpected character " ^ Char.escaped c) }
