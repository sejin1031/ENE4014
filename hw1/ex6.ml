type btree = Empty | Node of int * btree * btree

let rec balanced node =
  let rec height node = 
    match node with
      | Empty -> 0
      | Node(n,l,r) -> if height(l) > height(r) then height(l) + 1 else height(r) + 1
  in
  match node with
    | Empty -> true
    | Node(n,l,r) -> height(r) = height(l) && balanced(l) && balanced(r)