/* header */
%{
  open Ast
%}
%token LBRACKET RBRACKET
%token LPAREN RPAREN
%token LCURLY RCURLY
%token SEMICOL COMMA QUOTES
%token EVAL INF SUP
%token TRUE FALSE 
%token IF THEN ELSE FI
%token DEFINE FUNREC
%token CLASS METHOD NEW
%token LET IN
%token EQUAL PLUS MINUS STAR DOT
%token ARRAY GETARRAY SETARRAY
%token FOR TO DO DONE
%token EOF
%token <int> NUM
%token <string> IDENT

%right SEMICOL EQUAL
%left PLUS MINUS INF SUP STAR DOT

  /* types */
%start prog
%start def
%type <Ast.progra> prog
%type <Ast.class_declaration> def
%%

  /* grammar */
prog:
| LBRACKET RBRACKET {ASTProg([])}
| LBRACKET statements RBRACKET {ASTProg($2)}
; 

statements:
| dec SEMICOL {[$1]}
| dec SEMICOL statements {$1::$3}
;

dec:
| EVAL expr {ASTEval($2)}
| DEFINE IDENT expr {ASTVar($2, $3)}
| DEFINE IDENT LPAREN idents RPAREN EQUAL exprs {ASTFun($2, $4, $7)}
| FUNREC IDENT LPAREN idents RPAREN EQUAL exprs {ASTFun($2, $4, $7)}
| CLASS IDENT LCURLY RCURLY {ASTClass($2, [])}
| CLASS IDENT LCURLY defs RCURLY {ASTClass($2, $4)}
;

defs:
| def SEMICOL {[$1]}
| def SEMICOL defs {$1::$3}
;

def:
| DEFINE IDENT {ASTField($2)}
| METHOD IDENT LPAREN RPAREN LCURLY exprs SEMICOL RCURLY {ASTMethod($2, [], $6)}
| METHOD IDENT LPAREN idents RPAREN LCURLY exprs SEMICOL RCURLY {ASTMethod($2, $4, $7)}
;

exprs:
| expr {[$1]}
| expr COMMA exprs {$1::$3}
;

expr:
| NUM {ASTNum($1)}
| TRUE {ASTBool(true)}
| FALSE {ASTBool(false)}
| QUOTES IDENT QUOTES {ASTString($2)}
| IDENT {ASTIdent($1)}
| INF expr SUP DOT IDENT {ASTRef($2, $5)}
| expr EQUAL expr {ASTAff($1, $3)}
| expr PLUS expr {ASTPlus($1, $3)}
| expr MINUS expr {ASTMinus($1, $3)}
| expr STAR expr {ASTStar($1, $3)}
| expr INF expr {ASTInf($1, $3)}
| expr SUP expr {ASTSup($1, $3)}
| IF expr THEN expr SEMICOL ELSE expr SEMICOL FI {ASTIf($2, $4, $7)}
| LET IDENT expr IN {ASTLet($2, $3)}
| IDENT LPAREN exprs RPAREN {ASTApp($1, $3)}
| expr DOT NEW LPAREN RPAREN {ASTNew($1)}
| expr DOT IDENT LPAREN RPAREN {ASTAppMethod($1, $3, [])}
| expr DOT IDENT LPAREN exprs RPAREN {ASTAppMethod($1, $3, $5)}
| ARRAY LBRACKET expr RBRACKET {ASTArr($3)}
| SETARRAY LPAREN expr COMMA expr COMMA expr RPAREN {ASTAdd($3, $5, $7)}
| GETARRAY LPAREN expr COMMA expr RPAREN {ASTGetArray($3, $5)}
| FOR IDENT EQUAL expr TO expr DO exprs DONE {ASTFor($2, $4, $6, $8)}
;

idents:
| IDENT {[$1]}
| IDENT COMMA idents {$1::$3}
;

%%
(* end of grammar *)
