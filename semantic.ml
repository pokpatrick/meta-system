open Ast;;
open Environnement;;
open Metaclass;;

let rec eval_expr exp env =
  match exp with
    | ASTNum (n) -> Num (n)
    | ASTBool (b) -> Bool (b)
    | ASTString (s) -> Symbols (s)
    | ASTIdent (s) -> retrive_from_env env s
    | ASTRef (obj, e) ->
	(match eval_expr obj env with
	   | Instance (inst) -> retrive_from_inst inst e
	   | _ -> failwith "ERROR NOT INSTANCE")
    | ASTAff (v, e) ->
	let result = eval_expr e env in
	  (match v with
	     | ASTIdent (s) -> affect_in_env env s result
	     | ASTRef (obj, e) ->
		 (match eval_expr obj env with
		    | Instance (inst) -> affect_in_inst inst e result
		    | _ -> failwith "ERROR NOT INSTANCE")
	     | _ -> failwith "ERROR NOT POSSIBLE")
    | ASTPlus (e1, e2) ->
	(match (eval_expr e1 env, eval_expr e2 env) with
	   | (Num (n1), Num (n2)) -> Num (n1 + n2)
	   | _ -> failwith "OPERATION BETWEEN A ??? AND A NUMBER")
    | ASTMinus (e1, e2) ->
	(match (eval_expr e1 env, eval_expr e2 env) with
	   | (Num (n1), Num (n2)) -> Num (n1 - n2)
	   | _ -> failwith "OPERATION BETWEEN A ??? AND A NUMBER")
    | ASTStar (e1, e2) ->
	(match (eval_expr e1 env, eval_expr e2 env) with
	   | (Num (n1), Num (n2)) -> Num (n1 * n2)
	   | _ -> failwith "OPERATION BETWEEN A ??? AND A NUMBER")
    | ASTInf (e1, e2) ->
	(match (eval_expr e1 env, eval_expr e2 env) with
	   | (Num (n1), Num (n2)) ->
	       if (n1 <= n2) then
		 Bool (true)
	       else 
		 Bool (false)
	   | _ -> failwith "OPERATION BETWEEN A ??? AND A NUMBER")
    | ASTSup (e1, e2) ->
	(match (eval_expr e1 env, eval_expr e2 env) with
	   | (Num (n1), Num (n2)) ->
	       if (n1 >= n2) then
		 Bool (true)
	       else 
		 Bool (false)
	   | _ -> failwith "OPERATION BETWEEN A ??? AND A NUMBER")
    | ASTIf (guard, s1, s2) ->
	(match eval_expr guard env with
	   | Bool (true) -> eval_expr s1 env
	   | Bool (false) -> eval_expr s2 env
	   | _ -> failwith "GUARD NOT A BOOLEAN")
    | ASTLet (s, v) ->
	let result = eval_expr v env in
	let _ = record_value_env env s result in
	  result
    | ASTApp (fname, args) ->
	(match retrive_from_env env fname with
	   | Fun (sl, el) ->
	       let env' = eval_call sl args env in
	       let result = eval_exprs el env' in
		 result
	   | _ -> failwith "APPLICATION MUST BE A FUNCTION")
    | ASTNew (i_of) ->
	(match i_of with
	   | ASTIdent (s) ->
	       (match retrive_from_env env s with
		  | Class (c) ->
		      let representation =
			{fsize = 0;
			 field_value = Array.make environnement_size (Num (-3));
			 clazz = Some (c)
			} in
			(match c.field_value.(0) with
			   | Array (arr) ->
			       for i = 0 to Array.length arr - 1 do
				 (match arr.(i) with
				    | Symbols (s) when s <> "" -> representation.field_value.(i) <- (Num (-1))
				    | _ -> ())
			       done;
			   | _ -> failwith "ERROR IN INSTANCE CREATION");
			Instance (representation)
		  | _ -> failwith "INSTANTIATION MUST BE A CLASS")
	   | _ -> failwith "INSTANTIATION MUST BE A VALID CLASS NAME")
    | ASTAppMethod (i_name, m_name, args) ->
	(match eval_expr i_name env with
	   | Instance (i) as inst ->
	       (match i.clazz with
		  | None -> failwith "NON"
		  | Some (e) ->
		      (match retrive_from_repr e m_name with
			 | Method (sl, expls) ->
			     let method_env = eval_call sl args env in
			     let _ = record_value_env method_env "SELF" inst in
			     let result = eval_exprs expls method_env in
			       result
			 | MetaMethod (fonction) ->
			     fonction e "z1"
 			 | _ -> failwith "METHOD NOT FOUND"))
	   | _ -> failwith "UNKNOWN INSTANCE")
    | ASTArr (size) ->
	(match eval_expr size env with
	   | Num (n) -> Array (Array.make n (Num (0)))
	   | _ -> failwith "NOT A VALID SIZE")
    | ASTAdd (aname, pos, el) ->
	(match (eval_expr aname env, eval_expr pos env) with
	   | (Array (arr), Num (p)) when p < Array.length arr ->
	       Array.set arr p (eval_expr el env);
	       Bool (false)
	   | _ -> failwith "ARRAY PROBLEME SETTING")
    | ASTGetArray (aname, pos) ->
	(match (eval_expr aname env, eval_expr pos env) with
	   | (Array (arr), Num (p)) when p < Array.length arr ->
	       Array.get arr p
	   | _ -> failwith "ARRAY PROBLEME GETTING")
    | ASTFor (index, n, m, exprl) ->
	(match (eval_expr n env, eval_expr m env) with
	   | (Num (a), Num (b)) ->
	       for i = a to b do
		 let env' = record_value_env env index (Num (i)) in
		 let _ = eval_exprs exprl env' in ()
	       done;
	       Bool (false)
	   | _ -> failwith "NOT A NUMBER")
	  
and eval_exprs expl env =
  match expl with
    | [] -> failwith "NO EXPRESSION"
    | [head] -> eval_expr head env
    | head::tail -> 
	let _ = eval_expr head env in
	  eval_exprs tail env
	    
and eval_call parameters real_values env =
  let new_environnement =
    {fsize = 0;
     field_value = Array.make environnement_size (Num (-4));
     clazz = Some (env)
    } in
    new_environnement.field_value.(0) <- (Num (0)); (*index*)
    new_environnement.field_value.(1) <- (Array (Array.make environnement_size (Num (0)))); (*symbols*)
    new_environnement.field_value.(2) <- (Array (Array.make environnement_size (Num (0)))); (*values*)
    let _ = List.fold_left2 record_value_env new_environnement parameters (List.map (fun v -> eval_expr v env) real_values) in
      new_environnement
;;

let update_class_repr decr repr = 
  match decr with
    | ASTField (fname) ->
	record_field_repr repr fname
    | ASTMethod (mname, args, cel) ->
	let result = (Method (args, cel)) in
	  record_method_repr repr mname result
;;

let rec updates_class_repr decrs repr =
  match decrs with
    | [] -> repr
    | head::tail ->
	let _ = update_class_repr head repr in
	updates_class_repr tail repr
;;

let build_class_repr decrs =
  let representation =
    {fsize = 0;
     field_value = Array.make environnement_size (Num (0));
     clazz = Some (initial_meta_class)
    } in
    for i = 0 to Array.length initial_meta_class.field_value - 1 do
      if initial_meta_class.field_value.(i) <> Symbols ("") then
	representation.field_value.(i) <- (Array (Array.make environnement_size (Symbols (""))))
    done;
    updates_class_repr decrs representation
;;

let eval_statement s env =
  match s with 
    | ASTEval (exp) ->
	let result = eval_expr exp env in
	  print_endline (string_of_value result);
	  result
    | ASTVar (vname, exp) ->
	let result = eval_expr exp env in
	let _ = record_value_env env vname result in
	  result
    | ASTFun (fname, args, expl) ->
	let result = (Fun (args, expl)) in
	let _ = record_value_env env fname result in
	  result
    | ASTClass (cname, declarations) ->
	let result = Class (build_class_repr declarations) in  
	let _ = record_value_env env cname result in
	  result
;;

let rec eval_statements s_list env =
  match s_list with
    | [] -> env
    | head::tail ->
	let _ = eval_statement head env in 
	  eval_statements tail env
;;

let rec eval_prog prog =
  match prog with
    | ASTProg (p) ->
	let result = build_class_repr [ASTField ("index"); ASTField ("symbols"); ASTField ("values")] in
	let representation =
	  {fsize = 0;
	   field_value = Array.make environnement_size (Num (-2));
	   clazz = Some (result)
	  } in
	  representation.field_value.(0) <- (Num (0)); (*index*)
	  representation.field_value.(1) <- (Array (Array.make environnement_size (Symbols ("")))); (*symbols*)
	  representation.field_value.(2) <- (Array (Array.make environnement_size (Num (-2)))); (*values*)
	  eval_statements p representation
;;
