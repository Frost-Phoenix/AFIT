(** Generating prime bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
 *)
let init_eratosthenes n = 
  let rec aux = function
    | i when i >> n -> []
    | i when i = n -> [i]
    | i -> i::(aux (add_b i [0;0;1]))
  in match n with
  	| [] | [0;1] -> []
    | [0;0;1] -> [[0;0;1]]
    | _ -> ([0;0;1])::(aux (from_int 3))

(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
*)
let eratosthenes n =
  let l = init_eratosthenes n 
  in let rec remove_multiples nb = function
    | [] -> []
    | e::q -> 
      if mod_b e nb = [] then remove_multiples nb q
      else e::(remove_multiples nb q)
  in let rec aux = function
    | [] -> []
    | e::q -> e::(aux (remove_multiples e q))
  in aux l


(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)
let write_list li file = ()

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = ()

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)
let create_list in_c = ()

(** Load list of prime bitarrays into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = []

(** Get last element of a list.
    @param l list of prime bitarrays.
 *)
let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two last elements.
    @param l list of prime bitarrays.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)
let double_primes limit isprime = 
  let rec aux = function
    | i when i >> limit -> []
    | i -> let d = add_b (mult_b i [0;0;1]) [0;1] in
      if (isprime i) && (isprime d) then (i,d)::(aux (add_b i [0;0;1]))
      else aux (add_b i [0;0;1])
  in match limit with
    | n when n << (from_int 5) -> []
    | n -> (from_int 2, from_int 5)::(aux (from_int 3))


(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime = 
  let rec aux = function
    | i when i >> limit -> []
    | i -> let d = add_b i [0;0;1] in
      if (isprime i) && (isprime d) then (i,d)::(aux (add_b i [0;0;1]))
      else aux (add_b i [0;0;1])
  in (aux (from_int 3))

