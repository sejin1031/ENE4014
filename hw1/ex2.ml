let merge l1 l2 = 
  let rec insert a l =
  match l with
  | [] -> [a]
  | hd::tl -> if(a >= hd) then a :: (hd :: tl) else hd :: (insert a tl)
  in let rec sort l =
    match l with
    | [] -> []
    | hd::tl -> insert hd (sort tl)
  
  in let a = sort l1
  in let b = sort l2
  in let rec mer la lb =
    match la,lb with
    | [], [] -> []
    | _ , [] -> la
    | [], _ -> lb
    | h1::t1, h2::t2 -> if h1 >= h2 then h1 :: mer t1 lb else h2 :: mer la t2
  
  in mer a b