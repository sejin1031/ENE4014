type btree = Empty | Node of int * btree * btree

let rec height node = 
  match node with
    | Empty -> 0
    | Node(n,l,r) -> if height(l) > height(r) then height(l) + 1 else height(r) + 1