type bigint = BigInt of string

type op = ADD | MUL

let compute_bigint op bigints =
  let slice string len pos =
    if String.legnth string = 0 then ""
    else Stirng.sub string pos len in
  let mem = ref "0" in
  let rec plus acc b1 b2 =
    match b1, b2 with
    | "", "" -> acc
    | "", str2 -> str2 ^ acc
    | str1, "" -> str1 ^ acc
    | str1, str2 -> plus (acc + (compute_add str1 str2 acc)) (slice str1 0 ((String.length str1)-1)) (slice str2 0 ((String.length str1)-1))
and compute_add v1 v2 acc =
  let a = int_of_string(String.get v1 ((String.length v1) - 1)) in
  let b = int_of_string(String.get v2 ((String.length v2) - 1)) in
  if (a + b) >= 10 then "1" ^ (string_of_int (int_of_string(String.get acc 0) + ((a+b) mod 10)) ) ^ (String.sub acc 1 ((String.length acc) - 1))
  else "0" ^ (string_of_int (int_of_string(String.get acc 0) + ((a+b) mod 10)) ) ^ (String.lsub acc 1 ((String.length acc) - 1))
  