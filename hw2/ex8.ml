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