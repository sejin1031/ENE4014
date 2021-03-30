let rec length l =
  match l with
  | [] -> 0
  | hd::tl -> 1 + length tl;;

let rec append l1 l2 =
  match l1 with
  | [] -> l2
  | hd::tl -> hd:: (append tl l2)

let rec reverse l = 
  match l with
  | [] -> []
  | hd :: tl -> (reverse tl) @ [hd]
  
let rec nth l n =
  match l with
  | [] -> raise (Failure "list is too short")
  | hd::tl -> 
    match n with
    | 0 -> hd
    | _ -> nth tl (n-1)

let rec remove_first a l =
  match l with
  | [] -> []
  | hd::tl -> if (a = 0) then tl else hd::(remove_first (a-1) tl)

let rec insert a l =
  match l with
  | [] -> [a]
  | hd::tl -> if(a <= hd) then a :: (hd :: tl) else hd :: (insert a tl)

let rec sort l =
  match l with
  | [] -> []
  | hd::tl -> insert hd (sort tl)


let rec map f l =
  match l with
  | [] -> []
  | hd::tl -> (f hd) :: (map f tl)

let rec sum l =
  match l with
  | [] -> 0
  | hd::tl -> hd + (sum tl)

let rec prod l =
  match l with
  | [] -> 1
  | hd::tl -> hd * (prod tl)

let rec fold f l a =
  match l with
  | [] -> a
  | hd::tl -> f hd (fold f tl a)

let length l = fold (fun x y -> 1 + y) l 0

let reverse l = fold(fun x y -> y @ [x]) l []

let is_all_pos l = fold(fun x y -> (x > 0) && y) l true


let compose f g = fun x -> f(g(x))

let empty_map = fun x -> raise (Failure "not exist");;

let add_map (k,v) map =
  fun x -> if (k = x) then v else (map x);;

let m = (compose (add_map(1,"one")) (add_map(2,"two")) empty_map)

let a = 1;;

a;;