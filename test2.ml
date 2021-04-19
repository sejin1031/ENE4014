type bigint = BigInt of string

type op = ADD | MUL

let compute_bigint op bigints =
  let mem = ref "0" in
  let rec 