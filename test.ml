type exp = X | INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

let calculate exp = 
  let cur_val = ref 1.1 in
  let rec cal exp =
  match exp with
  | X -> !cur_val
  | INT x -> float_of_int x
  | REAL x -> x
  | ADD (e1, e2) -> cal e1 +. cal e2
  | SUB (e1, e2) -> cal e1 -. cal e2
  | MUL (e1, e2) -> cal e1 *. cal e2
  | DIV (e1, e2) -> cal e1 /. cal e2
  | SIGMA (s ,e, ex) -> (sigma s e ex 0.0)
  | INTEGRAL (a,b,c) -> 0.0
  and sigma current last exp acc=
    if (cal current) > (cal last) then (acc)
    else (
      cur_val := cal(current);
      sigma (ADD(current, INT 1)) last exp (acc +. (cal exp))
    )
    in cal exp





type real = float
exception Error
type exp = X
	| INT of int
	| REAL of real
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

let mathemadiga exp = 
	let rec eval exp env = 
		match (exp, env) with
    	  (X, []) -> raise Error
		| (X, hd::tl) -> hd
	    | (INT i, _) -> i
	    | (REAL r, _) -> r
	    | (ADD (e1, e2), env) -> 
			let v1 = eval e1 env in
			let v2 = eval e2 env in
			  v1 + v2
	    | (SUB (e1, e2), env) -> 
			let v1 = eval e1 env in
			let v2 = eval e2 env in
			  v1 - v2
	    | (MUL (e1, e2), env) -> 
			let v1 = eval e1 env in
			let v2 = eval e2 env in
			  v1 * v2
	    | (DIV (e1, e2), env) -> 
			let v1 = eval e1 env in
			let v2 = eval e2 env in
			  if v2 = 0 then raise Error
			  else v1 / v2
	    | (SIGMA (e1, e2, e3), env) ->
			let v1 = eval e1 env in
			let v2 = eval e2 env in
			  if v1 > v2 then raise Error
			  else if (v2 - v1) < 1 then (eval e3 (v1::env))
			  else 
			    (eval e3 (v1::env)) + (eval (SIGMA (INT (v1 + 1), INT v2, e3)) env)
	    	in
    eval exp []  