(* HW 1 *)

let rec gcd n m = 
    let n,m = if (n >= m) then (n,m) else (m, n) in
    if (m = 0) then n
    else gcd (n - m) m

let rec merge lst1 lst2 =
    match lst1, lst2 with
    | [], _ -> lst2
    | _, [] -> lst1
    | hd1::tl1, hd2::tl2 -> 
	if hd1 > hd2 then hd1 :: (merge tl1 lst2)
	else hd2 :: (merge lst1 tl2)

let rec range lower upper = 
	if (lower > upper) then [] 
	else lower :: (range (lower + 1) upper)

type formula = TRUE | FALSE
    | NOT of formula
    | ANDALSO of formula * formula
    | ORELSE of formula * formula
    | IMPLY of formula * formula
    | LESS of expr * expr
  and expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr
	
let rec calc exp = 
	match exp with 
	| PLUS (exp1, exp2) -> (calc exp1) + (calc exp2)
	| MINUS (exp1, exp2) -> (calc exp1) - (calc exp2)
	| NUM n -> n   

let rec eval form =
	match form with 
	| TRUE -> true 
	| FALSE -> false
	| NOT form' -> not (eval form')
	| ANDALSO (form1, form2) -> (eval form1) && (eval form2) 
	| ORELSE (form1, form2) -> (eval form1) || (eval form2)
	| IMPLY (form1, form2) -> (not (eval form1)) || (eval form2)  
	| LESS (expr1, expr2) -> (calc expr1) < (calc expr2)

type btree = Empty | Node of int * btree * btree

let t1 = Node (1, Empty, Empty)
let t2 = Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty))
let t3 = Node (1, Node (2, Node (3, Empty, Empty), Empty), Empty)

let rec height t = 
	match t with 
	| Empty -> 0 
	| Node (_, l, r) -> 
		if (height l) > (height r) then (height l) + 1
		else (height r) + 1 

let rec balanced t = 
	match t with 
	| Empty -> true 
	| Node (_, l, r) -> 
	  (balanced l) && (balanced r) && ((abs ((height l) - (height r))) <= 1)

let rec fold3 f a blst clst dlst = 
	match blst, clst, dlst with 
	| hd1 :: tl1, hd2 :: tl2, hd3 :: tl3 -> 
		fold3 f (f a hd1 hd2 hd3) tl1 tl2 tl3 
	| [], [], [] -> a 
	| _ -> raise (Failure "fail")  

let rec iter n f = 
	if n = 0 then fun x -> x 
	else 
		(fun x -> f ((iter (n-1) f) x)) 
		
let rec sigma (a,b,f) = 
	if a > b then 0 
	else 
		(f a) + sigma (a+1, b, f) 
		
let rec cartesian alst blst =
	let rec cartesian_sub a blst = 
		match blst with [] -> [] | hd :: tl -> (a, hd) :: (cartesian_sub a tl)
	in
	match alst with 
	| hd::tl -> (cartesian_sub hd blst) @ (cartesian tl blst) 
	| [] -> [] 