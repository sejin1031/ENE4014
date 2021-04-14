type exp = X | INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

(* let rec calculate exp = 
  match exp with
  | X 
  | INT x -> Int x
  | REAL x -> Float x
  | ADD (e1, e2) -> (calculate e1) + (calculate e2)
  | SUB (e1, e2) -> (calculate e1) - (calculate e2) *)

type bigint = BigInt of string
type op = ADD | MUL

(* let compute_bigint op b1 b2 = *)



let count_string s x = 
  let len = (String.length x) in
    let rec str s x count = 
      if s = "" ||  ((String.length s) < len) then count
      else if String.equal (String.sub s 0 len) x
      then str (String.sub s 1 ((String.length s) - 1)) x (count + 1)
      else str (String.sub s 1 ((String.length s) - 1)) x count
    in str s x 0

let rec pascal (a,b) =
  if a = b  || b = 0 then 1
  else pascal(a-1,b) + pascal(a-1,b-1)


let closest str strlist =
   let slice s pos len =
      if (String.length s) <= 1 then ""
      else (String.sub s pos len) in
   let rec dist a b =
    if (a = "") then (String.length b)
    else if (b = "") then (String.length a)
    else if (String.get a 0) = (String.get b 0) then dist (slice a 1 ((String.length a) - 1)) (slice b 1 ((String.length b) - 1))
    else (1 + min (min (dist (slice a 1 ((String.length a) - 1)) (slice b 1 ((String.length b) - 1)))
                (dist a (slice b 1 ((String.length b) - 1))) ) 
                (dist (slice a 1 ((String.length a) - 1)) b));
    
    in 
    let rec clo s sl best best_content = 
    match sl with
    | [] -> best_content
    | hd::tl -> if((dist s hd) < best) then clo str tl (dist s hd) hd
                else clo str tl best best_content in
      clo str strlist Int.max_int ""



let equals v1 v2 = 
    v1 = v2

let test t1 t2 answer =
  let v = closest t1 t2 in
  (equals v answer)



type relationships = (string * string) list



let likes relationships person =
  let result = ref [] in
  let find p rel = List.map(fun (_,x)-> x) (List.filter (fun (u,_) -> p = u) relationships) in
  let insert el = if List.mem el !result then result := !result else result := el::!result in
    result := (find person relationships);
    let rec bfs list checkedlist =
      match list with
      [] -> result := !result
      | hd::tl -> let tmplist = find hd relationships in
                List.map insert tmplist;
             bfs (tl@(List.filter(fun x -> (not (List.mem x checkedlist))) tmplist)) (checkedlist@tmplist)
    in bfs !result []; List.length !result
      

let selflove relationships =
  let like relationships person =(
    let result = ref [] in
    let find p rel = List.map(fun (_,x)-> x) (List.filter (fun (u,_) -> p = u) relationships) in
    let insert el = if List.mem el !result then result := !result else result := el::!result in
      result := (find person relationships);
      let rec bfs list checkedlist =
      match list with
      [] -> result := !result
      | hd::tl -> let tmplist = find hd relationships in
                List.map insert tmplist;
             bfs (tl@(List.filter(fun x -> (not (List.mem x checkedlist))) tmplist)) (checkedlist@tmplist)
      in bfs !result []; !result); in

    let self = ref [] in
    let startList = List.map(fun (x,_) -> x) relationships in
      let rec f l = 
        match l with
        [] -> self := !self
        | hd::tl ->   let tmp = like relationships hd in
                      let tmplist = List.filter (fun x -> List.length (List.filter (fun (a,b) -> a = x && b = hd) relationships) > 0) tmp in
                      if (List.length tmplist) > 0  && not (List.mem hd !self) then (self := hd :: !self ; f tl)else (self := !self; f tl)
        in f startList; List.length !self
        