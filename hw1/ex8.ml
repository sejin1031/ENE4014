let rec iter n f = 
  fun x -> 
    if n = 1 then f(x) else iter (n-1) f(f(x))