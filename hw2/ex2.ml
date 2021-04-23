type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

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