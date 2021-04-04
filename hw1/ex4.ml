type formula = TRUE | FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let rec eval form=
    match form with
      | TRUE -> true
      | FALSE -> false
      | NOT x -> not(eval(x))
      | ANDALSO (x, y) -> eval(x) && eval(y)
      | ORELSE (x, y) -> eval(x) || eval(y)
      | IMPLY (x,y) -> not(eval(x)) || eval(y)
      | LESS (exp1, exp2) -> exp(exp1) < exp(exp2)
  and exp form = 
    match form with
    | NUM x -> x
    | PLUS (x,y) -> exp(x) + exp(y)
    | MINUS (x,y) -> exp(x) - exp(y)