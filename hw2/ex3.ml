type bigint = BigInt of string
type op = ADD | MUL

let rec compute_bigint : op -> bigint * bigint -> bigint = 
  fun op (b1,b2) ->
  let slice str pos len = 
    if (String.length str) = 1 then ""
    else String.sub str pos len;
  in
  let string_of_bigint bigint =
  match bigint with
  | BigInt x -> x;
  in
  let bigint_of_string string =
    BigInt string
  in
  let rec compute_ADD str1 str2 acc =
    match str1, str2 with
    | "","" -> if (String.get acc 0) = '0' then slice acc 0 ((String.length acc) - 1) else acc
    | a, "" -> begin
        let temp = int_of_string (String.make 1 (String.get a ((String.length a) - 1))) + int_of_string (String.make 1 (String.get acc 0)) in
        if temp >= 10 
        then compute_ADD (slice a 0 ((String.length a) - 1)) "" (string_of_int temp)^(slice acc 1 ((String.length acc)-1))
        else compute_ADD (slice a 0 ((String.length a) - 1)) "" "0"^(string_of_int temp)^(slice acc 1 ((String.length acc)-1))
        end
    | "", a -> begin
      let temp = int_of_string (String.make 1 (String.get a ((String.length a) - 1))) + int_of_string (String.make 1 (String.get acc 0)) in
        if temp >= 10 
        then compute_ADD "" (slice a 0 ((String.length a) - 1)) (string_of_int temp)^(slice acc 1 ((String.length acc)-1)) 
        else compute_ADD "" (slice a 0 ((String.length a) - 1)) "0"^(string_of_int temp)^(slice acc 1 ((String.length acc)-1))
    end
    | a, b -> begin
      let temp = int_of_string (String.make 1 (String.get a ((String.length a) - 1))) + int_of_string (String.make 1 (String.get b ((String.length b) - 1))) + int_of_string (String.make 1 (String.get acc 0)) in
        if temp >= 10 
        then compute_ADD (slice a 0 ((String.length a) - 1)) (slice b 0 ((String.length b) - 1)) (string_of_int temp)^(slice acc 1 ((String.length acc)-1)) 
        else compute_ADD (slice a 0 ((String.length a) - 1)) (slice b 0 ((String.length b) - 1)) "0"^(string_of_int temp)^(slice acc 1 ((String.length acc)-1))
        end
  in
  let rec mult str1 str2 acc= 
   match str1 with
   "" -> acc
  | _ -> begin 
    let temp = int_of_string (String.make 1 (String.get str1 ((String.length str1) - 1))) * int_of_string (str2) + int_of_string (String.make 1 (String.get acc 0)) in
    if temp >= 10
    then mult (slice str1 0 ((String.length str1) - 1)) str2 (string_of_int temp)^(slice acc 1 ((String.length acc)-1))
    else mult (slice str1 0 ((String.length str1) - 1)) str2 "0"^(string_of_int temp)^(slice acc 1 ((String.length acc)-1))
    end
    
  in
  let rec compute_MULT str1 str2 acc = 
    match str1, str2 with
    | _, "" -> "0"
    | s1, s2 -> string_of_bigint (
      compute_bigint 
          ADD (bigint_of_string (compute_MULT s1 (slice s2 0 ((String.length s2) - 1)) acc^"0" ),
          bigint_of_string (mult str1 (String.make 1 (String.get str2 ((String.length s2) - 1 ))) acc))
      )
      
  in
    begin
    match op with
    | ADD -> bigint_of_string (compute_ADD (string_of_bigint b1) (string_of_bigint b2) "0")
    | MUL -> let temp = (compute_MULT (string_of_bigint b1) (string_of_bigint b2) "0") in
      if (String.get temp 0) = '0' then bigint_of_string(slice temp 1 ((String.length temp) - 1)) else bigint_of_string temp

    end


