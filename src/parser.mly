%{
open Lang
open Expression
open Types
open Signature
%}

%token <Expression.int_token> INT
%token <Expression.float_token> FLOAT
%token <Expression.bool_token> BOOL
%token <Expression.symbol_token> VAR
%token <Expression.symbol_token> CONSTRUCTOR

%token <Expression.symbol_token> LPAREN		(* ( *)
%token <Expression.symbol_token> RPAREN		(* ) *)
%token <Expression.symbol_token> PLUS		(* + *)
%token <Expression.symbol_token> MINUS		(* - *)
%token <Expression.symbol_token> AST		(* * *)
%token <Expression.symbol_token> DIVIDE		(* / *)

%token <Expression.symbol_token> TINT		(* int *)
%token <Expression.symbol_token> TFLOAT		(* float *)
%token <Expression.symbol_token> TBOOL		(* bool *)
%token <Expression.symbol_token> TUNIT		(* unit *)

%token <Expression.symbol_token> SMALLEREQUAL	(* <= *)
%token <Expression.symbol_token> IF	  	(* if *)
%token <Expression.symbol_token> THEN		(* then *)
%token <Expression.symbol_token> ELSE		(* else *)
%token <Expression.symbol_token> LET		(* let *)
%token <Expression.symbol_token> BE		(* = *)
%token <Expression.symbol_token> IN		(* in *)
%token <Expression.symbol_token> FUN		(* fun *)
%token <Expression.symbol_token> OUTPUT		(* -> *)
%token <Expression.symbol_token> FIX		(* fix *)
%token <Expression.symbol_token> COLON		(* : *)

%token <Expression.symbol_token> COMMA		(* , *)
%token <Expression.symbol_token> FST		(* fst *)
%token <Expression.symbol_token> SND		(* snd *)

%token <Expression.symbol_token> EMPTYLIST	(* [] *)
%token <Expression.symbol_token> LSQBRACKET	(* [ *)
%token <Expression.symbol_token> RSQBRACKET	(* ] *)
%token <Expression.symbol_token> DBCOLON	(* :: *)
%token <Expression.symbol_token> HEAD		(* hd *)
%token <Expression.symbol_token> TAIL		(* tl *)
%token <Expression.symbol_token> EMPTY		(* empty *)

%token <Expression.symbol_token> REF		(* ref *)
%token <Expression.symbol_token> UPDATE		(* := *)
%token <Expression.symbol_token> DEREF		(* ! *)
%token <Expression.symbol_token> SEMICOLON	(* ; *)
%token <Expression.symbol_token> LANGBRACKET	(* < *)
%token <Expression.symbol_token> RANGBRACKET 	(* > *)

%token <Expression.symbol_token> WHILE		(* while *)
%token <Expression.symbol_token> DO		(* do *)
%token <Expression.symbol_token> END		(* end *)

%token <Expression.symbol_token> TYPE		(* type *)
%token <Expression.symbol_token> TAG		(* | *)
%token <Expression.symbol_token> OF		(* of *)

%token <Expression.symbol_token> MATCH		(* match *)
%token <Expression.symbol_token> WITH		(* with *)

%token EOF

%right IN
%nonassoc SEMICOLON
%right OUTPUT
%right LET
%nonassoc IF ELSE WHILE
%nonassoc UPDATE
%right HEAD TAIL EMPTY FST SND
%nonassoc REF
%nonassoc COLON
%right DBCOLON
%left  RANGBRACKET SMALLEREQUAL
%left  PLUS MINUS
%left  AST DIVIDE
%nonassoc INT FLOAT BOOL EMPTYLIST VAR LPAREN DEREF

%start <Signature.signature list * Expression.exp> prog

%%

sigma:
| t=TYPE x=VAR BE c=cases			 { [(x.value, c)] }
| s1=sigma t=TYPE x=VAR BE c=cases		 { List.append s1 [(x.value, c)] }

cases:
| TAG x=CONSTRUCTOR				 { [(x.value, [])] }
| TAG x=CONSTRUCTOR OF l=modtypes		 { [(x.value, l)] }
| c1=cases TAG x=CONSTRUCTOR OF l=modtypes	 { List.append c1 [(x.value, l)] }
| c1=cases TAG x=CONSTRUCTOR 			 { List.append c1 [(x.value, [])] }

modtypes:
| t=types					 { [t] }
| l=modtypes t=types				 { List.append l [t]  }

prog:
| e=exp EOF					 { ([], e) }
| s=sigma e=prog			    	 { (List.append s (fst e), snd e) }
| EOF	  			    	    	 { failwith "Empty file" }

exp:
| t=LPAREN RPAREN
  { {value=EUnit; pos=t.pos} }
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
| t=MINUS n=INT 
  { {value=EInt {value= - n.value; pos=n.pos}; pos=t.pos} }
| t=MINUS f=FLOAT 
  { {value=EFloat {value= -. f.value; pos=f.pos}; pos=t.pos} }
| s=FUN LPAREN x=VAR COLON t1=types RPAREN COLON t2=types OUTPUT e=exp
  { {value=EFun({v={v=x; typ=t1}; typ=t2}, e); pos=s.pos} }
| s=FIX f=VAR LPAREN x=VAR COLON t1=types RPAREN COLON t2=types OUTPUT e=exp
  { {value=EFix(f, {v={v=x; typ=t1}; typ=t2}, e); pos=s.pos} }
| e1=exp PLUS e2=exp			    
  { {value=EAdd(e1, e2); pos=e1.pos} }
| e1=exp MINUS e2=exp			    
  { {value=ESubtract(e1, e2); pos=e1.pos} }
| e1=exp AST e2=exp		    
  { {value=EMultiply(e1, e2); pos=e1.pos} }
| e1=exp DIVIDE e2=exp		    
  { {value=EDivide(e1, e2); pos=e1.pos} }
| e1=exp SMALLEREQUAL e2=exp		    
  { {value=ELessthanorequal(e1, e2); pos=e1.pos} }
| e1=exp RANGBRACKET e2=exp
  { {value=EMorethan(e1, e2); pos=e1.pos} }
| s=IF e1=exp THEN e2=exp ELSE e3=exp
  { {value=EIf(e1, e2, e3); pos=s.pos} }
| s=LET x=VAR COLON t=types BE e2=exp IN e3=exp
  { {value=ELet({v=x; typ=t}, e2, e3); pos=s.pos} }
| e1=exp e2=exp
  { {value=EApply(e1, e2); pos=e1.pos} }
| t=LPAREN e1=exp COMMA e2=exp RPAREN
  { {value=EPair(e1, e2); pos=t.pos} }
| t=FST e=exp
  { {value=EFst e; pos=t.pos} }
| t=SND e=exp
  { {value=ESnd e; pos=t.pos} }
| s=EMPTYLIST COLON t=types
  { {value=EEmptylist t; pos=s.pos} }
| e1=exp DBCOLON e2=exp
  { {value=ECons(e1, e2); pos=e1.pos} }
| t=HEAD e=exp
  { {value=EHead e; pos=t.pos} }
| t=TAIL e=exp
  { {value=ETail e; pos=t.pos} }
| t=EMPTY e=exp
  { {value=EEmpty e; pos=t.pos} }
| t=REF e=exp
  { {value=ERef e; pos=t.pos} }
| e1=exp UPDATE e2=exp
  { {value=EUpdate(e1, e2); pos=e1.pos} }
| t=DEREF e=exp
  { {value=EDeref e; pos=e.pos} }
| e1=exp SEMICOLON e2=exp
  { {value=EIgnore(e1, e2); pos=e1.pos} }
| t=WHILE e1=exp DO e2=exp END
  { {value=EWhile(e1, e2); pos=t.pos} }
| x=CONSTRUCTOR LPAREN a=args RPAREN
  { {value=EConstructor(x.value, (List.rev a)); pos=x.pos} }
| x=CONSTRUCTOR
  { {value=EConstructor(x.value, []); pos=x.pos} }
| t=MATCH e=exp WITH p=patternmatch
  { {value=EMatch(e, p); pos=t.pos} }

args:
| e=exp						 { [e] }
| l=args COMMA e=exp				 { List.append l [e] }

types:
| LPAREN t=types RPAREN				 { t }
| TINT						 { TInt }
| TFLOAT			  		 { TFloat }
| TBOOL		  	   	  		 { TBool }
| TUNIT						 { TUnit }
| t1=types OUTPUT t2=types	  		 { TFun (t1, t2) }
| t1=types AST t2=types				 { TPair(t1, t2) }
| LSQBRACKET t=types RSQBRACKET			 { TList t }
| LANGBRACKET t=types RANGBRACKET		 { TRef t }
| x=VAR	      	      				 { TVariant x.value }

patternmatch:
| TAG p=pattern OUTPUT e=exp			 { [(p, e)] }
| pm=patternmatch TAG p=pattern OUTPUT e=exp	 { List.append pm [(p, e)] }

pattern:
| c=CONSTRUCTOR LPAREN v=variables RPAREN	 { (c.value, v) }

variables:
| v=VAR						 { [v] }
| l=variables COMMA v=VAR			 { List.append l [v] }