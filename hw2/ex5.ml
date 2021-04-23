let rec pascal (a,b) =
  if a = b  || b = 0 then 1
  else pascal(a-1,b) + pascal(a-1,b-1);;