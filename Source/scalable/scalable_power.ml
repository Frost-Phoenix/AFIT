(** Power function implementations for bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(** Shifts bitarray to the right by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift_r bA d = 
  let rec aux = function
    | [], _ -> []
    | l, 0  -> l
    | _::q, n -> aux (q, (n-1))
    in match bA with 
    | [] -> []
    | b::q -> 
        let res = match aux (q, d) with
            | [] -> []
            | l -> b::l
        in res


(** Logical and of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let and_b bA bB = 
    let rec aux = function 
        | [], _ -> []
        | _, [] -> []
        | x1::q1, x2::q2 -> (x1 land x2)::(aux (q1, q2))
    in match (bA, bB) with 
        | [], _ -> []
        | _, [] -> []
        | _::q1, _::q2 ->
            if q1 >>= q2 then 0::(aux (q1, q2))
            else 0::(aux (q2, q1))

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let pow x n =
  let rec aux a b = match (a, b) with
    | _, [] -> [0;1]
    | a, b -> mult_b a (aux a (diff_b b [0;1]))
  in aux x n

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let power x n = 
	let rec aux = function
    | (_, []) -> [0;1]
    | (a, b) ->
      let tmp = aux (a, (quot_b b [0;0;1])) in
      if (mod_b b [0;0;1]) = [] then (mult_b tmp tmp)
      else (mult_b a (mult_b tmp tmp))
  in aux (x, n)	


(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
let mod_power x n m =
  let rec aux = function 
    | _, e when (shift [0;1] e) >> n -> [0;1] 
    | a, e -> 
      let keep = (and_b (shift_r n e) [0;1]) = [0;1] 
      and nb = mod_b (mult_b a a) m in 
      if keep then mod_b (mult_b a (aux(nb, e+1))) m 
      else mod_b (aux(nb, e+1)) m 
  in 
    if x = [] then [] 
    else mod_b (aux(mod_b x m, 0)) m 


(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p = 
	let u = mod_b n (diff_b p [0;1]) 
  in mod_power x u p 
