let count_string s x = 
  let len = (String.length x) in
    let rec str s x count = 
      if s = "" ||  ((String.length s) < len) then count
      else if String.equal (String.sub s 0 len) x
      then str (String.sub s 1 ((String.length s) - 1)) x (count + 1)
      else str (String.sub s 1 ((String.length s) - 1)) x count
    in str s x 0;;


let equals v1 v2 = 
    v1 = v2

let test t1 t2 answer =
  let v = count_string answer t1 in
  (equals v t2)