open Ast;;
open Semantic;;

let parse_prog ic =
  let buffer = Lexing.from_channel ic in
    try
      Parser.prog Lexer.token buffer 
    with
      | Parsing.Parse_error -> 
	  (Printf.fprintf stderr "Syntaxe Error position %d\n" (buffer.Lexing.lex_curr_pos);
	   raise Parsing.Parse_error)
;;
 
let main file_name =
  let ic = open_in file_name in
    eval_prog (parse_prog ic)
;;

if Array.length(Sys.argv) < 2 then
  output_string stderr "./metapok <testfile>"
else
  ignore (main (Sys.argv.(1)));
flush stdout;
;;
