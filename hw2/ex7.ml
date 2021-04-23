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