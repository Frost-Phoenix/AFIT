(** Power function implementations for builtin integers *)

open Builtin
open Basic_arithmetics

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n = 
  let rec aux a b = match (a, b) with
    | _, 0 -> 1
    | a, b -> a * (aux a (b-1))
  in aux x n

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n = 
  let rec aux = function
    | (_, 0) -> 1
    | (a, b) ->
      let tmp = aux (a, (b / 2)) in
      if (modulo b 2) = 0 then tmp * tmp
      else a * tmp * tmp
  in aux (x, n)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m = 
  let rec aux = function 
    | (_, e) when (1 lsl e) > n -> 1
    | (a, e) ->
      let keep = (n lsr e) land 1 = 1
      and nb = (a * a) mod m in
        if keep then (a * aux(nb, e+1)) mod m
        else aux(nb, e+1) mod m
  in
    if x >= 0 then aux(x mod m, 0) mod m
    else aux(m + (x mod m), 0) mod m

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p = mod_power x n p


















