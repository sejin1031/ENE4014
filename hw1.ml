type formula = TRUE | FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

type btree = Empty | Node of int * btree * btree

let rec gcd n m = 
  if m == 0 then n
  else if n == m then n
    else if n > m then gcd (n-m) m
     else gcd n (m-n)


(* let rec merge l1 l2 =  *)
  

let rec range n m =
  if n > m then []
  else [n] @ range (n+1) m

(* let rec eval =
  fun form -> 
    match form with
      | TRUE -> true
      | FALSE -> false
      | NOT x -> not(eval(x))
      | ANDALSO (x, y) -> eval(x) && eval(y)
      | ORELSE (x, y) -> eval(x) || eval(y)
      | IMPLY (x,y) -> not(eval(x)) || eval(y)
      | LESS (exp1, exp2) -> 
        (match exp1 with
          | ) *)
    
    
  


let rec height node = 
  match node with
    | Empty -> 0
    | Node(n,l,r) -> if height(l) > height(r) then height(l) + 1 else height(r) + 1

let rec balanced node =
  match node with
    | Empty -> true
    | Node(n,l,r) -> if height(l) == height(r) then true else false


(* let rec fold3 f a b c d =
  match b with
  | [] -> a
  | hb::tb -> (match c with
    | [] -> a
    | hc::tc -> (match d with
    | hd::td -> fold3 (f f(a hb hc hd) tb tc td))) *)


let rec iter n f = 
  fun x -> 
    if n = 1 then f(x) else iter (n-1) f(f(x))

let rec sigma a b f =
  if a == b then f(b) else f(a) + sigma (a+1) b f

let rec cartesian l1 l2 =
  match l1 with
  | [] -> [] 
  | hd::tl -> (match l2 with
            | [] -> cartesian tl l2
            | h2::t2 ->  [(hd,h2)] @(cartesian l1 t2))
  






let rec contains_all l1 l2 =
	match l1 with
	| [] -> true
	| hd::tl -> if List.mem hd l2 then contains_all tl l2 else false

let equivalence a b = (contains_all a b) && (contains_all b a)

let test t1 t2 answer =
  let v = cartesian t1 t2 in
  (equivalence v answer)