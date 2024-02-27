(** Encoding Strings *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

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
let decode msg bits = ""