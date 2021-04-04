let rec range n m =
  if n > m then []
  else [n] @ range (n+1) m