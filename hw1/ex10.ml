let cartesian l1 l2 =
  let rec recursion ac aa bb =
    match aa, bb with 
    | [], _ -> ac
    | h1::t1, [] -> recursion ac t1 l2
    | h1::t1, h2::t2 -> recursion (ac @ [(h1,h2)]) aa t2
  in recursion [] l1 l2;;
  


  
let rec contains_all l1 l2 =
	match l1 with
	| [] -> true
	| hd::tl -> if List.mem hd l2 then contains_all tl l2 else false

let equivalence a b = (contains_all a b) && (contains_all b a)

let test t1 t2 answer =
  let v = cartesian t1 t2 in
  (equivalence v answer)