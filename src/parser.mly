%{
open Lang
%}

%token <Lang.int_token> INT
%token <Lang.float_token> FLOAT
%token <Lang.bool_token> BOOL

%token <Lang.symbol_token> LPAREN		(* ( *)
%token <Lang.symbol_token> RPAREN		(* ) *)
%token <Lang.symbol_token> PLUS			(* + *)
%token <Lang.symbol_token> MINUS		(* - *)
%token <Lang.symbol_token> MULTIPLY		(* * *)
%token <Lang.symbol_token> DIVIDE		(* / *)
%token <Lang.symbol_token> SMALLEREQUAL		(* <= *)
%token <Lang.symbol_token> IF	  		(* if *)
%token <Lang.symbol_token> THEN			(* then *)
%token <Lang.symbol_token> ELSE			(* else *)

%token EOF

%nonassoc ELSE
%right SMALLEREQUAL		 /* next lowest precedence */
%right PLUS MINUS		 /* medium precedence */
%right MULTIPLY DIVIDE           /* medium precedence */

%start <Lang.exp> prog

%%

prog:
  | e=exp EOF				    { e }
  | EOF	  			    	    {failwith "Empty file"}

exp:
  | LPAREN e=exp RPAREN	            
    { e }
  | n=INT				    
    { {value=EInt n; pos=n.pos} }
  | f=FLOAT				    
    { {value=EFloat f; pos=f.pos} }
  | b=BOOL			    
    { {value=EBool b; pos=b.pos} }
  | e1=exp PLUS e2=exp			    
    { {value=EAdd(e1, e2); pos=e1.pos} }
  | e1=exp MINUS e2=exp			    
    { {value=ESubtract(e1, e2); pos=e1.pos} }
  | e1=exp MULTIPLY e2=exp		    
    { {value=EMultiply(e1, e2); pos=e1.pos} }
  | e1=exp DIVIDE e2=exp		    
    { {value=EDivide(e1, e2); pos=e1.pos} }
  | e1=exp SMALLEREQUAL e2=exp		    
    { {value=ELessthanorequal(e1, e2); pos=e1.pos} }
  | t=IF e1=exp THEN e2=exp ELSE e3=exp	    
    { {value=EIf(e1, e2, e3); pos=t.pos} }