open M

(* exp environment : var -> exp *)
module EEnv = struct
  type t = var -> exp
  let empty = fun _ -> raise (Failure "Exp Env is empty")
  let extend (x,t) eenv = fun y -> if x = y then t else (eenv y)
  let find eenv x = eenv x
end

let rec poly: exp -> EEnv.t -> exp
 = fun exp eenv ->
 match exp with
 | TRUE -> TRUE
 | FALSE -> FALSE
 | CONST x -> CONST x
 | VAR x -> EEnv.find eenv x
 | ADD (e1, e2) -> ADD ((poly e1 eenv),(poly e2 eenv))
 | SUB (e1, e2) -> SUB ((poly e1 eenv),(poly e2 eenv))
 | MUL (e1, e2) -> MUL ((poly e1 eenv),(poly e2 eenv))
 | DIV (e1, e2) -> DIV ((poly e1 eenv),(poly e2 eenv))
 | ISZERO (e1) -> ISZERO (poly e1 eenv)
 | READ -> CONST (read_int())
 | IF (e1, e2, e3) -> IF ((poly e1 eenv), (poly e2 eenv), (poly e3 eenv))
 | LET (var, e1, e2) -> 
  begin
    let e3 = poly e1 eenv in
    poly e2 (EEnv.extend (var,e3) eenv)
  end
 (* | LETREC (f,x,e1,e2) -> 
 begin
  let tx = poly e1 eenv in
  let 
  
 end *)
 | PROC (var, e1) -> PROC (var, (poly e1 eenv))
 | CALL (e1, e2) -> CALL ((poly e1 eenv), (poly e2 eenv))

let expand: exp -> exp 
= fun exp -> 
  poly exp EEnv.empty
  (* raise (Failure "NotImplemented")		 *)


(* typeof: Do not modify this function *)
let typeof : exp -> typ 
=fun exp -> 
	let exp' = expand exp in 
	M.typeof exp'  
