let rec gcd n m = 
  if m == 0 then 0
  else if n == m then n
    else if n > m then gcd (n-m) m
     else gcd n (m-n)