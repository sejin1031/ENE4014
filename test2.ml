let slice s pos len =
      if (String.length s) <= 1 then ""
      else (String.sub s pos len);;
let rec dist a b =
    if (a = "") then (String.length b)
    else if (b = "") then (String.length a)
    else if (String.get a 0) = (String.get b 0) then dist (slice a 1 ((String.length a) - 1)) (slice b 1 ((String.length b) - 1))
    else (1 + min (min (dist (slice a 1 ((String.length a) - 1)) (slice b 1 ((String.length b) - 1)))
                (dist a (slice b 1 ((String.length b) - 1))) ) 
                (dist (slice a 1 ((String.length a) - 1)) b));;