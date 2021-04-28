type program = exp
and exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
  | NEWREF of exp 
  | DEREF of exp
  | SETREF of exp * exp
  | SEQ of exp * exp
  | BEGIN of exp
and var = string

type value = 
    Int of int 
  | Bool of bool 
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
  | Loc of loc
and loc = int
and env = (var * value) list
and mem = (loc * value) list

(* conversion of value to string *)
let value2str v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Loc l -> "Loc "^(string_of_int l)
  | Procedure (x,e,env) -> "Procedure "
  | RecProcedure (f,x,e,env) -> "RecProcedure "^f

(* environment *)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* memory *)
let empty_mem = [] 
let extend_mem (l,v) m = (l,v)::m
let rec apply_mem m l = 
  match m with
  | [] -> raise (Failure ("Location " ^ string_of_int l ^ " is unbound in Mem"))
  | (y,v)::tl -> if l = y then v else apply_mem tl l

(* use the function 'new_location' to generate a fresh memory location *)
let counter = ref 0
let new_location () = counter:=!counter+1;!counter

exception NotImplemented
exception UndefinedSemantics

(*****************************************************************)
(* TODO: Implement the eval function. Modify this function only. *)
(*****************************************************************)
let rec eval : exp -> env -> mem -> value * mem
=fun exp env mem -> 
  match exp with
  | CONST x -> (Int x,mem)
  | VAR x -> ((apply_env env x),mem)
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
  | ISZERO exp
      -> begin
      let (n, s) = eval exp env mem in
      (Bool (n = Int 0), s)
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
  | LET (var,exp1,exp2)
     -> begin
      let (v1, s1) = eval exp1 env mem in
      eval exp2 (extend_env (var,v1) env) s1
     end
  | LETREC (f,x,e1,e2)
    -> begin
      eval e2 (extend_env (f,(RecProcedure(f,x,e1,env))) env) mem
    end
  | PROC (var, exp) -> (Procedure (var,exp,env), mem)
  | CALL (exp1, exp2) ->
    begin
      let (e1, s1) = eval exp1 env mem in
      match e1 with
      Procedure (x,e,p1) -> begin
                            let (v,s2) = eval exp2 env s1 in
                            eval e (extend_env (x,v) p1) s2
                            end
      |RecProcedure (f,x,e,p1) -> 
                            begin
                            let (v,s2) = eval exp2 env s1 in
                            eval e (extend_env (f,e1) (extend_env (x,v) p1)) s2
                            end
    end
  (* | NEWREF exp ->
    begin
    let (v,s1) = eval exp env mem in
    let l = (new_location()) in
    (Loc l, (extend_mem (Loc l,v) s1))
    end
  | DEREF exp -> 
    begin
    let (l,s1) = eval exp env mem in
    ((apply_mem s1 l), s1)
    end
  | SETREF (exp1, exp2) ->
    begin
    let (l, s1) = eval exp1 env mem in
    let (v, s2) = eval exp2 env s1 in
    (v, (extend_mem (Loc l,v) s2))
    end *)
  | SEQ (exp1,exp) ->
    begin
    let (v1,s1) = eval exp1 env mem in
    eval exp env s1
    end
  | BEGIN exp ->
    begin
      eval exp env mem  
    end
  | READ -> (Int (read_int()),mem)
  | _ -> raise NotImplemented

(* driver code *)
let run : program -> value
=fun pgm -> (fun (v,_) -> v) (eval pgm empty_env empty_mem) 
