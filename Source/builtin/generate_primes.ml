(** Generating primes *)

open Builtin
open Basic_arithmetics

(** List composed of 2 and then odd integers starting at 3.
    @param n limit of list of odd integers, minimum value is 2.
 *)
let init_eratosthenes n = 
  let rec aux = function
    | i when i > n -> []
    | i when i = n -> [i]
    | i -> i::(aux (i+2))
  in match n with
    | 2 -> [2]
    | _ -> 2::(aux 3)

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let eratosthenes n = 
  let l = init_eratosthenes n 
  in let rec remove_multiples nb = function
    | [] -> []
    | e::q -> 
      if e mod nb = 0 then remove_multiples nb q
      else e::(remove_multiples nb q)
  in let rec aux = function
    | [] -> []
    | e::q -> e::(aux (remove_multiples e q))
  in aux l

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file = 
  let f = open_out file in
  let rec print_list = function
    | [] -> ()
    | e::q -> 
      Printf.fprintf f "%d\n" e; 
      print_list q
  in print_list li;
  close_out f
      
   

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = 
  let l = eratosthenes n in
  write_list l file

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = 
  let f = open_in file in
  let res = create_list f in
  close_in f;
  res
  
(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime = 
  let rec aux = function
    | i when i > limit -> []
    | i -> let d = (i * 2) + 1 in
      if (isprime i) && (isprime d) then (i,d)::(aux (i+2))
      else aux (i+2)
  in match limit with
    | n when n < 5 -> []
    | n -> (2,5)::(aux 3)

(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime = 
  let rec aux = function
    | i when i > limit -> []
    | i -> let d = i + 2 in
      if (isprime i) && (isprime d) then (i,d)::(aux (i+2))
      else aux (i+2)
  in (aux 3)























