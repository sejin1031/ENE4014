type program = exp
and exp = 
	| TRUE
	| FALSE
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
and var = string

exception TypeError

type typ = TyInt | TyBool 
	| TyFun of typ * typ (* t1 -> t2 *) 
	| TyVar of tyvar
and tyvar = string
type typ_eqn = (typ * typ) list (* t1 = t2 *)

let rec string_of_type ty = 
  match ty with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (t1,t2) -> "(" ^ string_of_type t1 ^ " -> " ^ string_of_type t2 ^ ")"
  | TyVar x -> x

let print_typ_eqns eqns = 
  List.iter (fun (ty1,ty2) -> print_string (string_of_type ty1 ^ " = " ^ string_of_type ty2 ^ "\n")) eqns;
  print_endline ""

(* type environment : var -> type *)
module TEnv = struct
  type t = var -> typ
  let empty = fun _ -> raise (Failure "Type Env is empty")
  let extend (x,t) tenv = fun y -> if x = y then t else (tenv y)
  let find tenv x = tenv x
end

(* substitution *)
module Subst = struct
  type t = (tyvar * typ) list
  let empty = []
  let find x subst = List.assoc x subst

  (* walk through the type, replacing each type variable by its binding in the substitution *)
  let rec apply : typ -> t -> typ
  =fun typ subst ->
    match typ with
    | TyInt -> TyInt
    | TyBool -> TyBool 
    | TyFun (t1,t2) -> TyFun (apply t1 subst, apply t2 subst)
    | TyVar x -> 
      try find x subst
      with _ -> typ

  (* add a binding (tv,ty) to the substitution and propagate the information *)
  let extend tv ty subst = 
    (tv,ty) :: (List.map (fun (x,t) -> (x, apply t [(tv,ty)])) subst)

  let print : t -> unit
  =fun subst -> 
      List.iter (fun (x,ty) -> print_endline (x ^ " |-> " ^ string_of_type ty)) subst
end

let tyvar_num = ref 0

(* generate a fresh type variable *)
let fresh_tyvar () = (tyvar_num := !tyvar_num + 1; (TyVar ("t" ^ string_of_int !tyvar_num)))

let rec gen_equations : TEnv.t -> exp -> typ -> typ_eqn 
=fun tenv e ty -> 
  match e with 
  | TRUE -> [(ty,TyBool)]
  | FALSE -> [(ty, TyBool)]
  | CONST x -> [(ty, TyInt)]
  | VAR x -> [(ty, TEnv.find tenv x)]
  | ADD (e1, e2) -> 
    begin
      let r1 = [(ty, TyInt)] in
      let r2 = gen_equations tenv e1 TyInt in
      let r3 = gen_equations tenv e2 TyInt in
      r1@r2@r3
    end
  | SUB (e1, e2) -> 
    begin
      let r1 = [(ty, TyInt)] in
      let r2 = gen_equations tenv e1 TyInt in
      let r3 = gen_equations tenv e2 TyInt in
      r1@r2@r3
    end
  | MUL (e1, e2) -> 
    begin
      let r1 = [(ty, TyInt)] in
      let r2 = gen_equations tenv e1 TyInt in
      let r3 = gen_equations tenv e2 TyInt in
      r1@r2@r3
    end
  | DIV (e1, e2) -> 
    begin
      let r1 = [(ty, TyInt)] in
      let r2 = gen_equations tenv e1 TyInt in
      let r3 = gen_equations tenv e2 TyInt in
      r1@r2@r3
    end
  | ISZERO exp -> 
    begin
    let r1 = [(ty, TyBool)] in
    let r2 = gen_equations tenv exp TyInt in
    r1@r2
    end
  | READ -> [(ty,TyInt)]
  | IF (e1,e2,e3) ->
    begin
    let r1 = gen_equations tenv e1 TyBool in
    let r2 = gen_equations tenv e2 ty in
    let r3 = gen_equations tenv e3 ty in
    r1@r2@r3
    end
  | LET (x, e1, e2) ->
    begin
      let t = fresh_tyvar () in
      let r2 = gen_equations tenv e1 t in
      let r3 = gen_equations (TEnv.extend (x,t) tenv) e2 ty in
      r2@r3
    end
  | LETREC (fx, x, e1, e2) ->
    begin
      let tx = fresh_tyvar () in
      let tfx = fresh_tyvar () in
      let tf = TyFun (tx, tfx) in
      let tenvf = TEnv.extend (fx,tf) tenv in
      let tenvxf = TEnv.extend (x, tx) tenvf in
      let r1 = gen_equations tenvxf e1 tfx in
      let r2 = gen_equations tenvf e2 ty in
      r1@r2
    end
  | PROC (var, e1) ->
  begin
    let t1 = fresh_tyvar () in
    let t2 = fresh_tyvar () in
    let r1 = [(ty, TyFun (t1, t2))] in
    let r2 = gen_equations (TEnv.extend (var, t1) tenv) e1 t2 in
    r1@r2
  end
  | CALL (e1, e2) ->
    begin
      let t = fresh_tyvar () in
      let r1 = gen_equations tenv e1 (TyFun (t,ty)) in
      let r2 = gen_equations tenv e2 t in
      r1@r2
    end


let rec unify: typ -> typ -> Subst.t -> Subst.t
 = fun t1 t2 s ->
  match (t1, t2) with
  | (TyInt, TyInt) -> s
  | (TyBool, TyBool) -> s
  | (TyVar a, TyVar b) -> if a = b then s else []
  | (t, TyVar x) -> unify (TyVar x) t s
  | (TyVar x, t) -> Subst.extend x t s
  | (TyFun (a1,a2), TyFun (b1, b2)) -> 
  begin
  let s1 = unify a1 b1 s in
  let t1 = Subst.apply a2 s1 in
  let t2 = Subst.apply b2 s1 in
  unify t1 t2 s1
  end
  | _ -> raise TypeError

let rec unifyall tyeqn sub =
  match tyeqn with
  | [] -> sub
  | (t1,t2)::u ->
    begin
      let s = unify (Subst.apply t1 sub) (Subst.apply t2 sub) sub in
      unifyall u s
    end

let solve : typ_eqn -> Subst.t
=fun eqns -> 
  unifyall eqns Subst.empty
  (* | _ -> raise TypeError  *)




(* typeof: Do not modify this function *)
let typeof : exp -> typ 
=fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations TEnv.empty exp new_tv in
  let _ = print_endline "= Equations = ";
          print_typ_eqns eqns in
  try 
    let subst = solve eqns in
    let ty = Subst.apply new_tv subst in
     print_endline "= Substitution = ";
      Subst.print subst;
      print_endline "";
      print_endline ("Type of the given program: " ^ string_of_type ty);
      print_endline "";
      ty
  with TypeError -> (print_endline "The program does not have type. Rejected."); exit (1)
