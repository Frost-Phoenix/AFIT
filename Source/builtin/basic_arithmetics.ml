(** Basic arithmetics with builtin integers *)

open Builtin

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero integer
*)
let rec gcd a b = match (a,b) with
	| (a, 0) -> abs a
	| (a, b) -> gcd b ((abs a) mod (abs b))

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b = 
	let rec aux x y =
        if x = 0 then (0, 1, y)
        else 
            let (u, v, d) = aux (y mod x) x in 
            (v - (y/x) * u, u, d)
    in aux a b
