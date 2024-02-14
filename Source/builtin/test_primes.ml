(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
let is_prime n = 
	let rec aux d = d * d > n || (n mod d <> 0 && aux (d + 2))
    in n = 2 || (n mod 2 <> 0 && aux 3) 
	(* 
	let rec aux = function
		| d when d*d > n -> true
		| d ->
			if n mod d = 0 then false
			else aux (d + 2)
	in match n with
		| 2 -> true
		| n when (n mod 2) = 0 -> false
		| n -> aux 3
	*)

(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)
let is_pseudo_prime p test_seq =
	let rec aux = function 
		| [] -> true
		| e::q -> (mod_power e p p) = e mod p && aux q
	in aux test_seq 
