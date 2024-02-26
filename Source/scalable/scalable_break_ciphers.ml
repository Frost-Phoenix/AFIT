(** Factoring bitarrays into primes *)

open Scalable
open Scalable_basic_arithmetics

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)
let break key = 
  let rec get_p = function 
    | (n, i) when (mod_b n i) = [] -> i
    | (n, i) -> get_p (n, (add_b i [0;0;1])) in
  let n, e = key in
  let p = get_p (n, from_int 3) in
  let q = quot_b n p in
  (p, q)

