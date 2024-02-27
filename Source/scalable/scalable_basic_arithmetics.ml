(** Basic arithmetics for ordered euclidian ring, case of bitarrays. *)

open Scalable

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let gcd_b bA bB = 
	let rec aux = function
	  | (l1, []) -> abs_b l1
	  | (l1, l2) -> aux (l2, (mod_b (abs_b l1) (abs_b l2)))
	in aux (bA, bB)

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let bezout_b bA bB =
	let rec aux x y =
    if x = [] then ([], [0;1], y)
    else 
	    let (u, v, d) = aux (mod_b y x) x in 
			(diff_b v (mult_b (quot_b y x) u), u, d)
   in aux bA bB

