{
open Lexing
open Parser
open Lang
open Expression

exception Lexer_error of string

let string_of_token (tok:Parser.token) : string =
match tok with
| INT n			->	string_of_int n.value ^ (locate n.pos)
| BOOL b 	   	-> 	string_of_bool b.value ^ (locate b.pos)
| FLOAT f 	   	-> 	string_of_float f.value ^ (locate f.pos)
| VAR x 	   	-> 	x.value ^ (locate x.pos)
| LPAREN s 	   	-> 	"'('" ^ (locate s.pos)
| RPAREN s 	   	-> 	"')'" ^ (locate s.pos)
| PLUS s 	   	-> 	"+" ^ (locate s.pos)
| MINUS s 	   	-> 	"-" ^ (locate s.pos)
| AST s 	   	-> 	"*" ^ (locate s.pos)
| DIVIDE s 	   	-> 	"/" ^ (locate s.pos)
| SMALLEREQUAL s   	-> 	"<=" ^ (locate s.pos)
| IF s 	 	   	-> 	"'if'" ^ (locate s.pos)
| THEN s 	   	-> 	"'then'" ^ (locate s.pos)
| ELSE s 	   	-> 	"'else'" ^ (locate s.pos)
| LET s 	   	-> 	"'let'" ^ (locate s.pos)
| BE s 		   	-> 	"=" ^ (locate s.pos)
| IN s 		   	-> 	"'in'" ^ (locate s.pos)
| FUN s 	   	-> 	"'fun'" ^ (locate s.pos)
| OUTPUT s 	   	-> 	"->" ^ (locate s.pos)
| FIX s  	   	-> 	"fix" ^ (locate s.pos)
| COLON s	   	-> 	":" ^ (locate s.pos)
| TINT	s	   	-> 	"int" ^ (locate s.pos)
| TFLOAT s	   	-> 	"float" ^ (locate s.pos)
| TBOOL s	   	-> 	"bool" ^ (locate s.pos)
| TUNIT s		->	"unit" ^ (locate s.pos)
| COMMA s		->	"," ^ (locate s.pos)
| FST s			->	"fst" ^ (locate s.pos)
| SND s			-> 	"snd" ^ (locate s.pos)
| EMPTYLIST s		->	"[]" ^ (locate s.pos)
| LSQBRACKET s		->	"[" ^ (locate s.pos)
| RSQBRACKET s		-> 	"]" ^ (locate s.pos)
| DBCOLON s  		->	"::" ^ (locate s.pos)
| HEAD s	  	->	"hd" ^ (locate s.pos)
| TAIL s		->	"tl" ^ (locate s.pos)
| EMPTY s		->	"empty" ^ (locate s.pos)
| REF s			->	"ref" ^ (locate s.pos)
| UPDATE s		->	":=" ^ (locate s.pos)
| DEREF s		-> 	"!" ^ (locate s.pos)
| SEMICOLON s		->	";" ^ (locate s.pos)
| LANGBRACKET s		->	"<" ^ (locate s.pos)
| RANGBRACKET s		->	">" ^ (locate s.pos)
| WHILE s     		->	"while" ^ (locate s.pos)
| DO s			-> 	"do" ^ (locate s.pos)
| END s			-> 	"end" ^ (locate s.pos)
| EOF 		   	-> 	""

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
| whitespace+	     	    { token lexbuf }
| newline+   		    { Lexing.new_line lexbuf; token lexbuf }
| digit+		    { INT	   ({value = (create_int lexbuf); pos = lexbuf.Lexing.lex_start_p}) }
| float+		    { FLOAT 	   ({value = (create_float lexbuf); pos = lexbuf.Lexing.lex_start_p}) }
| "true" | "false"	    { BOOL   	   ({value = (create_bool lexbuf); pos = lexbuf.Lexing.lex_start_p}) }
| "("			    { LPAREN       ({value="(" ; pos=lexbuf.Lexing.lex_start_p}) }
| ")"			    { RPAREN       ({value=")" ; pos=lexbuf.Lexing.lex_start_p}) }
| "<"			    { LANGBRACKET  ({value="<"; pos=lexbuf.Lexing.lex_start_p})}
| ">"			    { RANGBRACKET  ({value=">"; pos=lexbuf.Lexing.lex_start_p})}
| "+"			    { PLUS         ({value="+" ; pos=lexbuf.Lexing.lex_start_p}) }
| "-"			    { MINUS        ({value="-" ; pos=lexbuf.Lexing.lex_start_p}) }
| "*"			    { AST	   ({value="*" ; pos=lexbuf.Lexing.lex_start_p}) }
| "/"			    { DIVIDE       ({value="/" ; pos=lexbuf.Lexing.lex_start_p}) }
| "<="        		    { SMALLEREQUAL ({value="<="; pos=lexbuf.Lexing.lex_start_p}) }
| "="			    { BE      	   ({value="=" ; pos=lexbuf.Lexing.lex_start_p}) }
| "->"			    { OUTPUT	   ({value="->"; pos=lexbuf.Lexing.lex_start_p})}
| "if"			    { IF 	   ({value="if" ; pos=lexbuf.Lexing.lex_start_p}) }
| "then"		    { THEN 	   ({value="then" ; pos=lexbuf.Lexing.lex_start_p}) }
| "else"		    { ELSE 	   ({value="else" ; pos=lexbuf.Lexing.lex_start_p}) }
| "let"       		    { LET	   ({value="let" ; pos=lexbuf.Lexing.lex_start_p})}
| "in"			    { IN	   ({value="in" ; pos=lexbuf.Lexing.lex_start_p})}
| "fun"			    { FUN	   ({value="fun" ; pos=lexbuf.Lexing.lex_start_p})}
| "fix"			    { FIX	   ({value="fix" ; pos=lexbuf.Lexing.lex_start_p})}
| "int"			    { TINT	   ({value="int" ; pos=lexbuf.Lexing.lex_start_p})}
| "float"		    { TFLOAT	   ({value="float" ; pos=lexbuf.Lexing.lex_start_p})}
| "bool"		    { TBOOL	   ({value="bool" ; pos=lexbuf.Lexing.lex_start_p})}
| "unit"		    { TUNIT	   ({value="unit" ; pos=lexbuf.Lexing.lex_start_p})}
| ":"			    { COLON	   ({value=":" ; pos=lexbuf.Lexing.lex_start_p})}
| "::"			    { DBCOLON	   ({value="::" ; pos=lexbuf.Lexing.lex_start_p})}
| ","			    { COMMA	   ({value=","; pos=lexbuf.Lexing.lex_start_p})}
| "fst"			    { FST	   ({value="fst"; pos=lexbuf.Lexing.lex_start_p})}
| "snd"			    { SND	   ({value="snd"; pos=lexbuf.Lexing.lex_start_p})}
| "[]"			    { EMPTYLIST    ({value="[]"; pos=lexbuf.Lexing.lex_start_p})}
| "["			    { LSQBRACKET   ({value="["; pos=lexbuf.Lexing.lex_start_p})}
| "]"			    { RSQBRACKET   ({value="]"; pos=lexbuf.Lexing.lex_start_p})}
| "hd"			    { HEAD	   ({value="hd"; pos=lexbuf.Lexing.lex_start_p})}
| "tl"			    { TAIL	   ({value="tl"; pos=lexbuf.Lexing.lex_start_p})}
| "empty"		    { EMPTY	   ({value="empty"; pos=lexbuf.Lexing.lex_start_p})}
| "ref"			    { REF	   ({value="ref"; pos=lexbuf.Lexing.lex_start_p})}
| ":="			    { UPDATE	   ({value=":="; pos=lexbuf.Lexing.lex_start_p})}
| "!"			    { DEREF	   ({value="!"; pos=lexbuf.Lexing.lex_start_p})}
| ";"			    { SEMICOLON	   ({value=";"; pos=lexbuf.Lexing.lex_start_p})}
| "while"		    { WHILE	   ({value="while"; pos=lexbuf.Lexing.lex_start_p})}
| "do"			    { DO	   ({value="do"; pos=lexbuf.Lexing.lex_start_p})}
| "end"			    { END	   ({value="end"; pos=lexbuf.Lexing.lex_start_p})}
| variable+		    { VAR  	   ({value=create_variable lexbuf ; pos=lexbuf.Lexing.lex_start_p})}
| _ as c 		    { raise @@ Lexer_error("Unexpected character " ^ Char.escaped c) }
