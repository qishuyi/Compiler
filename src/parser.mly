%{
open Evaluation
open Lang
%}

%token <Lang.int_token> INT
%token <Lang.float_token> FLOAT
%token <Lang.bool_token> BOOL
%token <Lang.symbol_token> VAR
%token <Lang.symbol_token> CONSTRUCTOR

%token <Lang.symbol_token> LPAREN		      (* ( *)
%token <Lang.symbol_token> RPAREN		      (* ) *)
%token <Lang.symbol_token> PLUS		      	  (* + *)
%token <Lang.symbol_token> MINUS		      (* - *)
%token <Lang.symbol_token> AST		      	  (* * *)
%token <Lang.symbol_token> DIVIDE		      (* / *)

%token <Lang.symbol_token> SMALLEREQUAL	 	  (* <= *)
%token <Lang.symbol_token> GREATEREQUAL       (* >= *)
%token <Lang.symbol_token> IF	  	          (* if *)
%token <Lang.symbol_token> THEN		      	  (* then *)
%token <Lang.symbol_token> ELSE		      	  (* else *)
%token <Lang.symbol_token> LET		      	  (* let *)
%token <Lang.symbol_token> EQ		          (* = *)
%token <Lang.symbol_token> IN		          (* in *)
%token <Lang.symbol_token> COLON		      (* : *)

%token <Lang.symbol_token> TINT		      	  (* int *)
%token <Lang.symbol_token> TFLOAT		      (* float *)
%token <Lang.symbol_token> TBOOL		      (* bool *)
%token <Lang.symbol_token> TUNIT		      (* unit *)

%token <Lang.symbol_token> COMMA		      (* , *)
%token <Lang.symbol_token> FST		      	  (* fst *)
%token <Lang.symbol_token> SND		      	  (* snd *)

%token <Lang.symbol_token> EMPTYLIST	      (* [] *)
%token <Lang.symbol_token> LSQBRACKET	      (* [ *)
%token <Lang.symbol_token> RSQBRACKET	      (* ] *)
%token <Lang.symbol_token> DBCOLON	      	  (* :: *)
%token <Lang.symbol_token> HEAD		      	  (* hd *)
%token <Lang.symbol_token> TAIL		      	  (* tl *)
%token <Lang.symbol_token> EMPTY		      (* empty *)

%token <Lang.symbol_token> REF		      	  (* ref *)
%token <Lang.symbol_token> UPDATE		      (* := *)
%token <Lang.symbol_token> DEREF		      (* ! *)
%token <Lang.symbol_token> SEMICOLON	      (* ; *)
%token <Lang.symbol_token> DBSEMI			  (* ;; *)
%token <Lang.symbol_token> LANGBRACKET	  	  (* < *)
%token <Lang.symbol_token> RANGBRACKET 	      (* > *)

%token <Lang.symbol_token> WHILE		      (* while *)
%token <Lang.symbol_token> DO		          (* do *)
%token <Lang.symbol_token> END		          (* end *)

%token <Lang.symbol_token> TYPE		          (* type *)
%token <Lang.symbol_token> TAG		          (* | *)
%token <Lang.symbol_token> OF		          (* of *)

%token <Lang.symbol_token> MATCH		      (* match *)
%token <Lang.symbol_token> WITH		          (* with *)
%token <Lang.symbol_token> ARROW			  (* -> *)

%token EOF

%left SEMICOLON
%left UPDATE
%left LANGBRACKET RANGBRACKET SMALLEREQUAL GREATEREQUAL EQ
%left PLUS MINUS
%left AST DIVIDE

%start <Lang.declaration> prog

%%
prog:
| ds=decllist EOF	                              		 			{ List.rev ds }

formal:
| LPAREN x=VAR COLON t=typ RPAREN                         			{ { name=x.value; typ=t } }

formalist:
| arg=formal                                   		 				{ [arg] }
| args=formalist arg=formal              		 			    	{ arg :: args }

constructor:
| TAG x=CONSTRUCTOR				               		 				{ {tag=x.value; fields=[]} }
| TAG x=CONSTRUCTOR OF ts=typelist		       		 				{ {tag=x.value; fields=List.rev ts} }

constructorlist:
| c=constructor 											 		{ [c] }
| cs=constructorlist c=constructor   		 						{ c :: cs }

decl:
| LET f=VAR args=formalist COLON t=typ EQ e=exp DBSEMI		 		{ FSignature (f.value, { name=f.value; ret_typ=t; args=List.rev args; body=e }) }
| LET f=VAR COLON t=typ EQ e=exp DBSEMI								{ FSignature (f.value, { name=f.value; ret_typ=t; args=[]; body=e}) }
| TYPE v=VAR EQ c=constructorlist DBSEMI						   	{ VSignature (v.value, { name=v.value; cases=List.rev c}) }

decllist:
| d=decl                                     		 				{ [d] }
| ds=decllist d=decl                           		 				{ d :: ds }

typ:
| LPAREN t=typ RPAREN				                 				{ t }
| TINT						                         				{ TInt }
| TFLOAT			  		                         				{ TFloat }
| TBOOL		  	   	  		                         				{ TBool }
| TUNIT						                         				{ TUnit }
| t1=typ AST t2=typ				                 					{ TPair(t1, t2) }
| LSQBRACKET t=typ RSQBRACKET			             				{ TList t }
| LANGBRACKET t=typ RANGBRACKET		             					{ TRef t }
| x=VAR	      	      				                 				{ TVariant x.value }

typelist:
| t=typ					                         					{ [t] }
| ts=typelist t=typ				                 					{ t :: ts  }

varlist:
| v=VAR                                              				{ [v] }
| vs=varlist COMMA v=VAR                           	 				{ v :: vs }

case:
| TAG c=CONSTRUCTOR LPAREN v=varlist RPAREN ARROW e=exp 			{ ((c.value, List.rev v), e) }
| TAG c=CONSTRUCTOR ARROW e=exp 									{ ((c.value, []), e)}

caselist:						
| c=case 															{ [c] }
| cs=caselist c=case	     										{ c :: cs }

exp:
| e=exp_app                                          				{ e }

exp_app:
| f=VAR es=expslist                             		 			{ {value=EApply (f, List.rev es); pos=f.pos} }
| e=exp_ops                                          				{ e }

expslist:
| e=exp_ops                                    	     				{ [e] }
| es=expslist e=exp_ops                        		 				{ e :: es }

exp_ops:
| e1=exp_ops PLUS e2=exp_ops                   			 			{ {value=EBinop(OAdd, e1, e2); pos=e1.pos} }
| e1=exp_ops MINUS e2=exp_ops                  		 				{ {value=EBinop(OSub, e1, e2); pos=e1.pos} }
| e1=exp_ops AST e2=exp_ops                    		 				{ {value=EBinop(OMul, e1, e2); pos=e1.pos} }
| e1=exp_ops DIVIDE e2=exp_ops                 		 				{ {value=EBinop(ODiv, e1, e2); pos=e1.pos} }
| e1=exp_ops LANGBRACKET e2=exp_ops            		 				{ {value=EBinop(OLt, e1, e2); pos=e1.pos} }
| e1=exp_ops RANGBRACKET e2=exp_ops            		 				{ {value=EBinop(OGt, e1, e2); pos=e1.pos} }
| e1=exp_ops GREATEREQUAL e2=exp_ops           		 				{ {value=EBinop(OGeq, e1, e2); pos=e1.pos} }
| e1=exp_ops SMALLEREQUAL e2=exp_ops           		 				{ {value=EBinop(OLeq, e1, e2); pos=e1.pos} }
| e1=exp_ops SEMICOLON e2=exp_ops              		 				{ {value=EIgnore(e1, e2); pos=e1.pos} }
| e1=exp_ops UPDATE e2=exp_ops                 		 				{ {value=EUpdate(e1, e2); pos=e1.pos} }
| t=REF e=exp_ops                              		 				{ {value=ERef e; pos=t.pos} }
| t=DEREF e=exp_ops                            		 				{ {value=EDeref e; pos=t.pos} }
| e=exp_base                                   		 				{ e }
		
expclist:
| e=exp                                        		 				{ [e] }
| es=expclist COMMA e=exp                      		 				{ e :: es }

exp_base:
| t=LPAREN RPAREN                                                   { {value=EUnit; pos=t.pos} }
| n=INT                                                             { {value=EInt n; pos=n.pos} }
| f=FLOAT                                                           { {value=EFloat f; pos=f.pos} }
| b=BOOL                                                            { {value=EBool b; pos=b.pos} }
| x=VAR                                                             { {value=EVar x; pos=x.pos} }
| LPAREN e=exp RPAREN                                               { e }
| s=IF e1=exp THEN e2=exp ELSE e3=exp                               { {value=EIf(e1, e2, e3); pos=s.pos} }
| s=LET x=VAR COLON t=typ EQ e2=exp IN e3=exp                       { {value=ELet(x, t, e2, e3); pos=s.pos} }
| t=LPAREN e1=exp COMMA e2=exp RPAREN                               { {value=EPair(e1, e2); pos=t.pos} }
| t=FST e=exp                                                       { {value=EFst e; pos=t.pos} }
| t=SND e=exp                                                       { {value=ESnd e; pos=t.pos} }
| s=EMPTYLIST COLON t=typ                                           { {value=EEmptylist t; pos=s.pos} }
| e1=exp DBCOLON e2=exp                                             { {value=ECons(e1, e2); pos=e1.pos} }
| t=HEAD e=exp                                                      { {value=EHead e; pos=t.pos} }
| t=TAIL e=exp                                                      { {value=ETail e; pos=t.pos} }
| t=EMPTY e=exp                                                     { {value=EEmpty e; pos=t.pos} }
| t=WHILE e1=exp DO e2=exp END                                      { {value=EWhile(e1, e2); pos=t.pos} }
| x=CONSTRUCTOR LPAREN args=expclist RPAREN                         { {value=EConstructor(x, (List.rev args)); pos=x.pos} }
| x=CONSTRUCTOR                                                     { {value=EConstructor(x, []); pos=x.pos} }
| t=MATCH e=exp WITH cs=caselist                                 	{ {value=EMatch(e, List.rev cs); pos=t.pos} }