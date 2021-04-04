let sigma a b f =
  let rec recursion acc a b f = 
  if a <= b then recursion (acc + f(a)) (a + 1) b f
  else acc
  in recursion 0 a b f