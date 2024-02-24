(** Testing for primality *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Deterministic primality test *)
let is_prime n = 
	let rec aux d = 
		(mult_b d d) >> n || ((mod_b n d) <> [] && aux (add_b d [0;0;1]))
	in n = [0;0;1] || ((mod_b n [0;0;1]) <> [] && aux [0;1;1]) 


(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays againt which to test
 *)
let is_pseudo_prime p test_seq =
  let rec aux = function 
    | [] -> true
    | e::q -> (mod_power e p p) = (mod_b e p) && aux q
  in aux test_seq 

