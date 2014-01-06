open Ast;;
open Environnement;;

let getClass env cname =
  print_endline cname;
  Class (env)
;;

let getFields env cname =
  print_endline cname;
  env.field_value.(0)
;;

let getMethods env cname =
  print_endline cname;
  env.field_value.(1)
;;

let addFields env fname =
  print_endline fname;
  record_field_repr env fname;
  env.field_value.(0)
;;

let ast_of_string string  =
  let buffer = Lexing.from_string  string in
    try
      Parser.def Lexer.token buffer 
    with
      | Parsing.Parse_error -> 
	  (Printf.fprintf stderr "Syntaxe Error position %d\n" (buffer.Lexing.lex_curr_pos);
	   failwith "NON")
;;

print_endline "HERE";;
ast_of_string "METHOD setY(n) { <SELF>.y1 = n; };";;

let modifyMethod env cname =
  let ast = ast_of_string cname in
    print_endline cname;
    env.field_value.(0)
;;
  
let initial_meta_class =
  let meta =
    {fsize = 0;
     field_value = Array.make environnement_size (Num (-1));
     clazz = None
    } in
  let arr = Array.make environnement_size (Symbols ("")) in
    arr.(0) <- Symbols ("field_name");
    arr.(1) <- Symbols ("method_name");
    arr.(2) <- Symbols ("method_body");
    
    meta.field_value.(0) <- Array (arr);
    meta.field_value.(1) <- Array (Array.make environnement_size (Symbols ("")));
    meta.field_value.(2) <- Array (Array.make environnement_size (Symbols ("")));

    (match (meta.field_value.(1), meta.field_value.(2)) with
       | (Array (arr1), Array (arr2)) -> 
	   (arr1.(0) <- Symbols ("getClass"); arr2.(0) <- MetaMethod (getClass);
	    arr1.(1) <- Symbols ("getFields"); arr2.(1) <- MetaMethod (getFields);
	    arr1.(2) <- Symbols ("getMethods"); arr2.(2) <- MetaMethod (getMethods);
	    arr1.(3) <- Symbols ("addField"); arr2.(3) <- MetaMethod (addFields);
	    arr1.(4) <- Symbols ("modifyMethod"); arr2.(4) <- MetaMethod (modifyMethod));
       | _ -> failwith "no");

    meta.clazz <- None (*Some (meta)*);
    meta
;;
