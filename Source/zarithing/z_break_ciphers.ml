(** Factoring big integers into primes *)

open Z

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let rec get_p = function 
    | (n, i) when n mod i = zero -> i
    | (n, i) -> get_p (n, (i - (of_int 2))) in
  let n, e = key in
  let p = get_p (n, sqrt n) in
  let q = n / p in
  (p, q)

