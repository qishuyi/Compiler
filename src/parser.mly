%{
open Lang
%}

%token <Lang.int_token> INT
%token <Lang.float_token> FLOAT
%token <Lang.bool_token> BOOL
%token <Lang.symbol_token> VAR

%token <Lang.symbol_token> LPAREN		(* ( *)
%token <Lang.symbol_token> RPAREN		(* ) *)
%token <Lang.symbol_token> PLUS			(* + *)
%token <Lang.symbol_token> MINUS		(* - *)
%token <Lang.symbol_token> MULTIPLY		(* * *)
%token <Lang.symbol_token> DIVIDE		(* / *)

%token <Lang.symbol_token> SMALLEREQUAL		(* <= *)
%token <Lang.symbol_token> GREATER		(* > *)
%token <Lang.symbol_token> IF	  		(* if *)
%token <Lang.symbol_token> THEN			(* then *)
%token <Lang.symbol_token> ELSE			(* else *)
%token <Lang.symbol_token> LET			(* let *)
%token <Lang.symbol_token> BE			(* = *)
%token <Lang.symbol_token> IN			(* in *)
%token <Lang.symbol_token> FUN			(* fun *)
%token <Lang.symbol_token> OUTPUT		(* -> *)
%token <Lang.symbol_token> FIX			(* fix *)
%token EOF

%left  GREATER SMALLEREQUAL
%left  PLUS MINUS
%left  MULTIPLY DIVIDE
%right OUTPUT

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
| x=VAR
  { {value=EVar x; pos=x.pos} }
| b=BOOL			    
  { {value=EBool b; pos=b.pos} }
| t=FUN x=VAR OUTPUT e=exp
  { {value=EFun(x, e); pos=t.pos} }
| t=FIX f=VAR x=VAR OUTPUT e=exp
  { {value=EFix(f, x, e); pos=t.pos} }
  
| t=MINUS f=FLOAT
  { {value=EFloat {value= -. f.value; pos=t.pos};pos=t.pos} }
| t=MINUS n=INT
  { {value=EInt {value= - n.value; pos=t.pos};pos=t.pos} }
  
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
| e1=exp GREATER e2=exp
  { {value=EMorethan(e1, e2); pos=e1.pos} }

| t=IF e1=exp THEN e2=exp ELSE e3=exp
  { {value=EIf(e1, e2, e3); pos=t.pos} }
| t=LET s=VAR BE e2=exp IN e3=exp
  { {value=ELet(s, e2, e3); pos=t.pos} }
| e1=exp e2=exp
  { {value=EApply(e1, e2); pos=e1.pos} }
    