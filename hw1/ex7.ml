let rec fold3 f a b c d =
  match b,c,d with
   | [], [], [] -> a
   | hb::tb, hc::tc, hd::td -> fold3 f (f a hb hc hd) tb tc td