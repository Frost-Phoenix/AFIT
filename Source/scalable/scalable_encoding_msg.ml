(** Encoding Strings *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Shifts bitarray to the right by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec sr1 bA d = 
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
  
(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)
let encode str bits = 
  let str_len = String.length str in
  let rec aux = function 
    | 0 -> []
    | i -> 
      let c = str.[str_len - i] in
      let nb = Char.code c in                   (* ascii code of c *)
      add_b (shift (from_int nb) ((i-1) * bits)) (aux(i-1))     (* put ascii code of c to the left of the result *)
  in aux str_len


(** Decode a string containing ASCII characters.
   @param msg is an integer representing an encoded message.
   @param bits number of bits on which to store a character ;
          alphanumeric ASCII is 7.
*)
let decode msg bits =
	let rec aux = function 
	  | n when sr1 msg (n * bits) = [] -> ""
	  | n -> 
	    let shift_m = (sr1 msg (n * bits)) in
	    let nb = and_b shift_m (from_int ((1 lsl bits) - 1)) in   
	    let c = String.make 1 (Char.chr (to_int nb)) in
	    (aux (n + 1)) ^ c
 in aux 0


