
{ 
  open Parser
  let line=ref 1

}

let ws = (['\t' ' ']*)
let eol = '\n'
let lparen = '('
let rparen = ')'
let lbracket = '['
let rbracket = ']'
let lcurly = '{'
let rcurly = '}'
let semicol = ";"
let quotes = "\""
let comma = ","
let dot = "."
let r_eval = "EVAL"
let r_true = "TRUE"
let r_false = "FALSE"
let r_if = "IF"
let r_then = "THEN"
let r_else = "ELSE"
let r_fi = "FI"
let r_var = "DEFINE"
let r_funrec = "FUNREC"
let r_class = "CLASS"
let r_method = "METHOD"
let r_new = "NEW"
let r_inf = "<"
let r_sup = ">"
let r_let = "LET"
let r_in = "IN"
let r_for = "FOR"
let r_to = "TO"
let r_do = "DO"
let r_done = "DONE"
let r_vararray = "ARRAY"
let r_getarray = "GETARRAY"
let r_setarray = "SETARRAY"
let plus = "+"
let minus = "-"
let star = "*"
let equal = '='
let ident = (['a'-'z''A'-'Z'] ['a'-'z''A'-'Z''0'-'9'])*
let digit = ['0'-'9']
let num = (digit*)

rule token = parse
| ws {token lexbuf}
| eol {incr line; token lexbuf}
| eof {EOF}
| lparen {LPAREN}
| rparen {RPAREN}
| lbracket {LBRACKET}
| rbracket {RBRACKET}
| lcurly {LCURLY}
| rcurly {RCURLY}
| semicol {SEMICOL}
| quotes {QUOTES}
| comma {COMMA}
| dot {DOT}
| r_eval {EVAL}
| r_true {TRUE}
| r_false {FALSE}
| r_if {IF}
| r_then {THEN}
| r_else {ELSE}
| r_fi {FI}
| r_var {DEFINE}
| r_funrec {FUNREC}
| r_class {CLASS}
| r_method {METHOD}
| r_new {NEW}
| r_let {LET}
| r_in {IN}
| r_for {FOR}
| r_to {TO}
| r_do {DO}
| r_done {DONE}
| r_vararray {ARRAY}
| r_getarray {GETARRAY}
| r_setarray {SETARRAY}
| r_inf {INF}
| r_sup {SUP}
| plus {PLUS}
| minus {MINUS}
| star {STAR}
| equal {EQUAL}
|(['a'-'z''A'-'Z'])(['a'-'z''A'-'Z''0'-'9'])* {IDENT(Lexing.lexeme lexbuf)}
| digit as n {NUM(int_of_string (Char.escaped n))}
| num as n {NUM(int_of_string n)}
| _ { failwith((Lexing.lexeme lexbuf) ^ ": mistake at line " ^ string_of_int !line)}
