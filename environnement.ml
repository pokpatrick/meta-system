open Ast;;

let environnement_size = 16;;

type value =
  | Num of int
  | Bool of bool
  | Symbols of string
  | Fun of string list * expr list
  | Class of instance_env
  | Instance of instance_env
  | Method of string list * expr list
  | MetaMethod of (instance_env -> string -> value)
  | Array of value array
      
and instance_env =
    {mutable fsize : int;
     field_value : value array;
     mutable clazz : instance_env option
    }
;;

let rec string_of_value v =
  match v with
    | Num (n) -> ("=> " ^ (string_of_int n))
    | Bool (true) -> "true!"
    | Bool (false) -> "false!"
    | Symbols (s) -> s
    | Fun (_) -> "it is a function"
    | Class (c) -> ("Class " ^ (string_of_value c.field_value.(0)) ^ (string_of_value c.field_value.(1)))
    | Instance (i) -> ("it is an instance " ^ (string_of_value i.field_value.(0)))
    | Method (_) -> "Method"
    | MetaMethod (_) -> "?????"
    | Array (values) ->
	let s = ref "" in 
	  for i = 0 to Array.length values - 1 do
	    s := !s ^ " " ^ (string_of_value values.(i))
	  done;
	  "[ " ^ !s ^ " ]"
;;

let record_field_repr repr fname =
  match repr.field_value.(0) with
    | Array (arr) ->
	let i = ref 0 in
	let found = ref false in
	  while (!i < Array.length arr) && not (!found) do
	    (match arr.(!i) with
	       | Symbols ("") ->
		   (found := true; 
		    arr.(!i) <- Symbols (fname))
	       | _ -> i := !i + 1)
	  done;
    | _ -> failwith ("RECORDING " ^ fname ^ " ON AN NON-Class_Repr")
;;

let record_method_repr repr mname value =
  match (repr.field_value.(1), repr.field_value.(2)) with
    | (Array (arr1), Array (arr2)) ->
	let i = ref 0 in
	let found = ref false in
	  while (!i < Array.length arr1) && not (!found) do
	    (match arr1.(!i) with
	       | Symbols ("") ->
		   (found := true; 
		    arr1.(!i) <- Symbols (mname);
		    arr2.(!i) <- value)
	       | _ -> i := !i + 1)
	  done;
    | _ -> failwith ("RECORDING " ^ mname ^ " ON AN NON-Class_Repr")
;;

let record_value_env env symbol value =
  match (env.field_value.(0), env.field_value.(1), env.field_value.(2)) with
    | (Num (n), Array (arr1), Array (arr2)) ->
	let i = n in
	  arr1.(i) <- Symbols (symbol);
	  arr2.(i) <- value;
	  env.field_value.(0) <- Num (i + 1);
	  env
    | _ -> failwith ("RECORDING " ^ symbol ^ " ON AN NON-ENVIRONNEMENT")
;;

let rec retrive_from_repr repr symbol =
  match (repr.field_value.(0), repr.field_value.(1), repr.field_value.(2)) with
    | (Array (arr0), Array (arr1), Array (arr2)) ->
	let found = ref (-1) in
	  for i = 0 to Array.length arr0 - 1 do
	    if arr0.(i) = Symbols (symbol) then
	      found := -2
	  done;
	  if !found = -2 then
	    Num (0)
	  else
	    (for i = 0 to Array.length arr1 - 1 do
	       if arr1.(i) = Symbols (symbol) then
		 found := i
	     done;
	     (match !found with
		| -1 -> (*failwith (symbol ^ "NOT FOUND !")*)
		    (match repr.clazz with
		       | None -> failwith (symbol ^ "NOT FOUND !")
		       | Some (m) -> retrive_from_repr m symbol)
		| _ -> arr2.(!found)))
    | _ -> failwith ("RETRIVING " ^ symbol ^ " ON AN NON-Class_Repr")
;;

let retrive_from_inst inst symbol =
  match inst.clazz with
    | None -> failwith "ERROR RETRIVING NO clazz"
    | Some (e) ->
	(match e.field_value.(0) with
	   | Array (arr) ->
	       let found = ref (-1) in
		 for i = 0 to Array.length arr - 1 do
		   if arr.(i) = Symbols (symbol) then
		     found := i
		 done;
		 (match !found with
		    | -1 -> retrive_from_repr e symbol
		    | _ -> inst.field_value.(!found))
	   | _ -> failwith ("RETRIVING " ^ symbol ^ " ON AN NON-INSTANCE"))
;;

let rec retrive_from_env env symbol =
  match (env.field_value.(0), env.field_value.(1), env.field_value.(2)) with
    | (Num (n), Array (arr1), Array (arr2)) ->
	let found = ref (-1) in
	  for i = 0 to Array.length arr1 - 1 do
	    if arr1.(i) = Symbols (symbol) then
	      found := i
	  done;
	  (match !found with
	     | -1 -> 
		 (match env.clazz with
		    | None -> failwith (symbol ^ "NOT FOUND IN ENV")                   (* GP META ! *)
		    | Some (e) ->
			(match (e.field_value.(0), e.field_value.(1), e.field_value.(2)) with
			   | (Array (arr0), Array (arr1), Array (arr2)) -> retrive_from_repr e symbol
			   | (Num (n), Array (arr1), Array (arr2)) -> retrive_from_env e symbol
			   | _ -> failwith "THAT IS NOT POSSIBLE"))
	     | _ -> arr2.(!found))
    | _ -> failwith ("RETRIVING " ^ symbol ^ " ON AN NON-ENVIRONNEMENT")
;;

let affect_in_inst inst name value =
  match inst.clazz with
    | None -> failwith "ERROR AFECTING IN clazz"
    | Some (e) ->
	(match e.field_value.(0) with
	   | Array (arr) ->
	       let found = ref (-1) in
		 for i = 0 to Array.length arr - 1 do
		   if arr.(i) = Symbols (name) then
		     found := i
		 done;
		 (match !found with
		    | -1 -> failwith "AFFECTATION NOT POSSIBLE"
		    | _ -> 
			inst.field_value.(!found) <- value;
			value)
	   | _ -> failwith ("AFFECTING " ^ name ^ " ON AN NON-INSTANCE"))
;;

let rec affect_in_env env symbol value =
  match (env.field_value.(0), env.field_value.(1), env.field_value.(2)) with
    | (Num (n), Array (arr1), Array (arr2)) ->
	let found = ref (-1) in
	  for i = 0 to Array.length arr1 - 1 do
	    if arr1.(i) = Symbols (symbol) then
	      found := i
	  done;
	  (match !found with
	     | -1 -> 
		 (match env.clazz with
		    | None -> failwith "variable NOT FOUND IN ENV LEVEL"
		    | Some (e) -> affect_in_env e symbol value)
	     | _ -> 
		 arr2.(!found) <- value;
		 value)
    | _ -> failwith ("AFFECTING " ^ symbol ^ " ON AN NON-ENVIRONNEMENT")
;;
