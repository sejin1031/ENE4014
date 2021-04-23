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
    in cal exp;

let rec diff: ae * string -> ae
 = fun (exp, var) ->
    match exp with
    |CONST a -> CONST 0
    |VAR x -> if x = var then CONST 1 else CONST 0
    |POWER (s, i) -> 
        if s = var then begin 
            if i = 0 then CONST 0
            else if i = 1 then CONST 1
            else TIMES[CONST i;POWER (s, i - 1)] 
            end
            else CONST 0
    |TIMES l ->
        begin
        match l with
            |[] -> CONST 0
            |h::t -> 
            begin
                if inspectTIMES l var false then 
                    begin
                    match t with
                    | [] -> TIMES (calculateTIMES [h] var 0)
                    | _ -> let temp = TIMES (calculateTIMES l var 0)
                            in if temp = TIMES [] then CONST 0
                            else temp
                    end
                else CONST 0
                
            end
        end
    |SUM m ->
        match m with
            | [] -> SUM []
            |h::t -> 
                begin
                match t with
                | [] -> diff(h,var)
                | _ -> SUM[diff(h, var); diff(SUM t, var)]
                end

and inspectTIMES expList var flag =
    match expList with
    | [] -> flag
    | hd::tl -> begin
        match hd with 
        | CONST a -> inspectTIMES tl var (flag)
        | VAR x -> if x = var then true else inspectTIMES tl var (flag)
        | POWER (s, i) -> if s = var then true else inspectTIMES tl var (flag)
        | TIMES l -> inspectTIMES l var flag
        | _ -> true
        end
and calculateTIMES list var count=
    match list with
    | [] -> if count > 0 then [diff (POWER(var,count), var)] else []
    | hd::tl -> begin 
                match hd with
                |CONST a -> [CONST a]@(calculateTIMES tl var count)
                |VAR x -> if x = var then [CONST 1]@(calculateTIMES tl var (count+1)) else [VAR x]@(calculateTIMES tl var count)
                |POWER (s, i) -> 
                    if s = var then (calculateTIMES tl var (count+i)) else [POWER(s,i)]@(calculateTIMES tl var count)
                | TIMES l -> (calculateTIMES l var count)@(calculateTIMES tl var count)
                | _ -> []
                end

(* let compute_bigint op b1 b2 = 
  let rec plus str1 str2 result
  match op with
  | ADD ->  *)



let count_string s x = 
  let len = (String.length x) in
    let rec str s x count = 
      if s = "" ||  ((String.length s) < len) then count
      else if String.equal (String.sub s 0 len) x
      then str (String.sub s 1 ((String.length s) - 1)) x (count + 1)
      else str (String.sub s 1 ((String.length s) - 1)) x count
    in str s x 0;

let rec pascal (a,b) =
  if a = b  || b = 0 then 1
  else pascal(a-1,b) + pascal(a-1,b-1);


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
      clo str strlist Int.max_int "";



let equals v1 v2 = 
    v1 = v2;

let test t1 t2 answer =
  let v = closest t1 t2 in
  (equals v answer);



type relationships = (string * string) list;



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
        