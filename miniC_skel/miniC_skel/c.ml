type program = exp
and exp =
	| SKIP
	| TRUE
	| FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
	| LE of exp * exp
	| EQ of exp * exp
	| NOT of exp 
  | IF of exp * exp * exp
	| WHILE of exp * exp 
	| LET of var * exp * exp
	| PROC of var list * exp 
	| CALLV of exp * exp list 
	| CALLR of exp * var list
	| ASSIGN of var * exp 
	| RECORD of (var * exp) list 
	| FIELD of exp * var
	| ASSIGNF of exp * var * exp 
  | READ of var
	| PRINT of exp 
  | SEQ of exp * exp
  | BEGIN of exp
and var = string

type value = 
    Int of int
  | Bool of bool
	| Unit
  | Procedure of var list * exp * env
	| Record of record
  | Loc of loc
and loc = int 
and env = (var * loc) list
and mem = (loc * value) list
and record = (var * loc) list

(* conversion of value to string *)
let value2str v =
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unit -> "."  
	| Procedure (params,e,env) -> "Procedure "
  | Record record -> "Record "
	| Loc l -> "Loc "^(string_of_int l)

(* environment *)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* memory *)
let empty_mem = [] 
let extend_mem (l,v) m = (l,v)::(List.filter (fun (l',_) -> l != l') m)
let rec apply_mem m l = 
  match m with
  | [] -> raise (Failure ("Location " ^ string_of_int (l) ^ " is unbound in Mem"))
  | (y,v)::tl -> if l = y then v else apply_mem tl l

let counter = ref 0
let new_location () = counter:=!counter+1; (!counter)

(* conversion of env to string *)
let string_of_env env = 
	List.fold_left (fun str (x,l) -> Printf.sprintf "%s\n%s -> %d" str x l) "" env  
(* conversion of mem to string *)
let string_of_mem mem = 
	List.fold_left (fun str (l,v) -> Printf.sprintf "%s\n%d -> %s" str l (value2str v)) "" mem 		
		
exception NotImplemented
exception UndefinedSemantics
(* if the following variable is set true, gc will work (otherwise, gc simply returns a given memory). *)
let remove_garbage = ref false 

let gc: env * mem -> mem
= fun (env, mem) ->
	if (not !remove_garbage) then mem 
	else
     
		raise NotImplemented

let rec eval : program -> env -> mem -> (value * mem)
=fun pgm env mem ->  
  match pgm with
  | READ x -> (Unit, extend_mem (apply_env env x, Int (read_int())) mem) (* Do not modify *)
	| PRINT e ->
		let v, mem' = eval e env mem in
		let _ = print_endline (value2str v) in
		(v, gc(env,mem')) (* Do not modify *) 
	| SKIP -> (Unit, mem)
	| TRUE -> (Bool true,mem)
	| FALSE -> (Bool false,mem)
	| CONST x -> (Int x,mem)
  | VAR x -> (Int (apply_env env x),mem)
  | ADD (exp1, exp2) 
      -> begin
        let (n1,s1) = eval exp1 env mem in
        let (n2,s2) = eval exp2 env s1 in
        let n1 = int_of_string (value2str n1) in
        let n2 = int_of_string (value2str n2) in
        ((Int(n1+n2)), s2)
        end
  | SUB (exp1, exp2) 
      -> begin
        let (n1,s1) = eval exp1 env mem in
        let (n2,s2) = eval exp2 env s1 in
        let n1 = int_of_string (value2str n1) in
        let n2 = int_of_string (value2str n2) in
        (Int (n1-n2), s2)
        end
  | MUL (exp1, exp2) 
      -> begin
        let (n1,s1) = eval exp1 env mem in
        let (n2,s2) = eval exp2 env s1 in
        let n1 = int_of_string (value2str n1) in
        let n2 = int_of_string (value2str n2) in
        (Int (n1*n2), s2)
        end
  | DIV (exp1, exp2) 
      -> begin
        let (n1,s1) = eval exp1 env mem in
        let (n2,s2) = eval exp2 env s1 in
        let n1 = int_of_string (value2str n1) in
        let n2 = int_of_string (value2str n2) in
        (Int (n1/n2), s2)
        end
	| LE (exp1, exp2)
			-> begin
			let (n1, s1) = eval exp1 env mem in
			let (n2, s2) = eval exp2 env s1 in
			if ((int_of_string (value2str n1)) <= (int_of_string (value2str n2))) then (Bool true, s2)
			else (Bool false, s2)
			end
	| EQ (exp1, exp2)
			-> begin
			let (n1, s1) = eval exp1 env mem in
			let (n2, s2) = eval exp2 env s1 in
			if ((int_of_string (value2str n1)) = (int_of_string (value2str n2))) then (Bool true, s2)
			else (Bool false, s2)
			end
	| NOT (exp)
		-> begin
		let (b,s1) = eval exp env mem in
		match b with
    | Bool true -> (Bool false,s1)
    | Bool false -> (Bool true,s1)
		end
  | IF (condition, t, f)
      -> begin
      let (flag, s1) = eval condition env mem in
      match flag with
        Bool true -> begin
                let (n,s2) = eval t env s1 in
                (n, s2)
                end
      | Bool false -> begin
                let (n,s2) = eval f env s1 in
                (n, s2)
                end
      end
	| WHILE (e1, e2) 
		-> begin
		let (flag, s0) = eval e1 env mem in
    match flag with
    Bool flag -> 
      begin
      if flag then begin let (v1, s1) = eval e2 env s0 in
                        let (v2,s2) = eval pgm env s1 in
                        (v2,s2)
                        end
      else (Unit, s0)
      end
		end
  | LET (var,exp1,exp2)
     -> begin
      let (v1, s1) = eval exp1 env mem in
      let l = (new_location()) in
      eval exp2 (extend_env (var,l) env) (extend_mem (l,v1) s1)
     end
  | PROC (var, exp) -> (Procedure (var,exp,env), mem)
	| ASSIGN (var, exp) ->
			begin
			let (v, s1) = eval exp env mem in
			(v, (extend_mem ((apply_env env var),v) s1))
			end
	| RECORD list ->
    begin
    let rec record list result mem =
      match list with
      | [] -> (RECORD result,mem)
      | hd::tl -> 
      begin
        let (var,exp) = hd in
        let (v,s) = eval exp env mem in
        let l = (new_location()) in
        record tl ([Loc l]@result) (extend_mem (l,v) s)
      end
      in
      record list [] mem
    end
  | FIELD (exp, var) ->
    begin
      let (r,s1) = eval exp env mem in
      ((apply_mem s1 (apply_env env r var)), s1)
    end
  | ASSIGNF (exp1, var, exp2) ->
    begin
      let (r,s1) = eval exp1 env mem in
      let (v,s2) = eval exp2 env s1 in
      (v, extend_mem ((apply_env r var),v), s2)
    end
  | SEQ (exp1,exp) ->
    begin
    let (v1,s1) = eval exp1 env mem in
    eval exp env s1
    end
  | BEGIN exp ->
    begin
      eval exp env mem  
    end
	| _ -> raise NotImplemented (* TODO *)


let run : program -> bool -> bool -> unit 
= fun pgm with_gc print_mem_size ->
	let _ = remove_garbage := with_gc in 
	let mem = snd (eval pgm empty_env empty_mem) in   
	if (print_mem_size) then 
		print_endline (Printf.sprintf "Final mem size: %d" (List.length mem))
	
	
