%{
open Lang
%}

%token <int> INT
%token <float> FLOAT
%token <bool> BOOL

%token LPAREN		(* ( *)
%token RPAREN		(* ) *)
%token PLUS		(* + *)
%token MINUS		(* - *)
%token MULTIPLY		(* * *)
%token DIVIDE		(* / *)
%token SMALLEREQUAL	(* <= *)
%token IF	  	(* if *)
%token THEN		(* then *)
%token ELSE		(* else *)

%token EOF

%right ELSE			 /* lowest precedence */
%right SMALLEREQUAL		 /* next lowest precedence */
%right PLUS MINUS		 /* medium precedence */
%right MULTIPLY DIVIDE           /* medium precedence */

%start <Lang.exp> prog

%%

prog:
  | e=exp EOF				    { e }
  | LPAREN e=exp RPAREN		    	    { e }
  | EOF	  				    {failwith "Empty file"}

exp:
  | n=INT				    { EInt n }
  | f=FLOAT				    { EFloat f }
  | b=BOOL				    { EBool b }
  | e1=exp PLUS e2=exp			    { EAdd (e1, e2) }
  | e1=exp MINUS e2=exp			    { ESubtract (e1, e2) }
  | e1=exp MULTIPLY e2=exp		    { EMultiply (e1, e2) }
  | e1=exp DIVIDE e2=exp 		    { EDivide (e1, e2) }
  | e1=exp SMALLEREQUAL e2=exp		    { ELessthanorequal (e1, e2) }
  | IF e1=exp THEN e2=exp ELSE e3=exp 	    { EIf (e1, e2, e3) }
