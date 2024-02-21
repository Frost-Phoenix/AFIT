(** Factoring Builtin int primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
	let rec get_p = function 
		| (n, i) when n mod i = 0 -> i
		| (n, i) -> get_p (n, (i - 2)) in
	let n, e = key in
	let p = get_p (n, (int_of_float(sqrt(float_of_int n)))) in
	let q = n / p in
	(p, q)
