type exp = X | INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp 
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

let rec calculate exp = 
  match exp with
  | X -> exp
  | INT x -> x
  | REAL x -> int_of_float x
  | ADD (e1, e2) -> (calculate e1) + (calculate e2)
  | SUB (e1, e2) -> (calculate e1) - (calculate e2)
  | MUL (e1, e2) -> (calculate e1) - (calculate e2)
  | DIV (e1, e2) -> (calculate e1) - (calculate e2)
  | SIGMA (e1, e2, e3) -> calculate (sigma e1 e2 e3 (INT 0))
and calsigma c ex=
	match ex with
	X -> calculate c
	| _ -> calculate ex
and sigma c e ex acc =
	if calculate(c) = calculate(e) then acc
	else sigma ADD(c, INT 1) e ex (ADD (acc, (calsigma c ex)))
