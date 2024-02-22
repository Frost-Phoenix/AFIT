(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
contexte zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code.

 *)

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let from_int x = 
	let rec aux = function
		| 0 -> []
		| n -> (n land 1)::(aux (n lsr 1))
	in match x with 
		| 0 -> []
		| x when x < 0 -> 1::(aux (abs x))
		| x -> 0::(aux x)

let from_int_n x = match from_int x with
	| [] -> []
	| x::q -> q	
			
(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)
let to_int bA = 
	let rec aux n = function
		| [] -> 0
		|	x::q -> 
			match x with
				| 0 -> aux (n + 1) q
				| x -> (1 lsl n) + aux (n + 1) q        (* x == 1 *)
	in match bA with 
		| [] -> 0
		| 0::q -> aux 0 q
		| x::q -> (-1) * (aux 0 q)    							(* x == 1 *)
			

(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA = 
	let rec aux = function
		| [] -> ""
		| x::q -> (aux q) ^ (string_of_int x)
	in match bA with
		| [] -> print_string "0"
		| 0::q -> print_string (aux q)
		| x::q -> print_string ("-" ^ (aux q))     	(* x == 1 *)

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Reverse 'a list
    @param  'a list
 *)
let reverse l = 
	let rec aux l2 = function
		| [] -> l2
		|	x::q -> aux (x::l2) q
	in aux [] l

(** List len of 'a list
    @param 'a list
 *)
let rec l_len = function
	| [] -> 0
	| x::q -> 1 + (l_len q)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 otherwise.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)
let rec compare_n nA nB = 
	let rec aux = function
		| (1::q1, 0::q2) ->  1
		| (0::q1, 1::q2) -> -1
		| (_::q1, _::q2) -> aux (q1, q2)
		| _ -> 0
	in match (l_len nA, l_len nB) with
		| (len1, len2) when len1 > len2 ->  1
		| (len1, len2) when len1 < len2 -> -1
		| _ -> aux (reverse nA, reverse nB)


(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB = if compare_n nA nB = 1 then true else false

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB = if compare_n nA nB = 1 then false else true

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB = 
	let r = compare_n nA nB in
	if r = 1 || r = 0 then true
	else false

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB = 
	let r = compare_n nA nB in
	if r = 1 || r = 0 then false
	else true


(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 otherwise.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB = 
	match (bA, bB) with
		|	([], []) 			 ->  0
		| (0::q1, 1::q2) ->  1
		| (1::q1, 0::q2) -> -1
		| (_::q1, _::q2) -> compare_n q1 q2
		| _ -> failwith "compare_b error"
		

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB = true

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB = true

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB = true

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB = true


(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA = 10

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA = []

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = if a < 2 then 0 else 1

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = if a = 1 || a = 3 then 1 else 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a, _mod_t a)

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB = []

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB = []

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB = []

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB = []

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d = []

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB = []

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let quot_b bA bB =  []

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB = []

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB = ([], [])
