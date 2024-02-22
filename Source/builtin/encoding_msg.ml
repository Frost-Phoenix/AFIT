(** Encoding Strings *)

open Builtin
open Basic_arithmetics
open Power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits = 
  let str_len = String.length str in
  let rec aux = function 
    | 0 -> 0
    | i  -> 
      let c = str.[str_len - i] in
      let nb = Char.code c in                   (* ascii code of c *)
      (nb lsl ((i - 1) * bits)) + aux (i-1)     (* put ascii code of c to the left of the result *)
  in aux str_len

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let decode msg bits = 
  let rec aux = function 
    | n when msg lsr (n * bits) = 0 || (n * bits) >= 64 -> ""
    | n -> 
      let shift_m = (msg lsr (n * bits)) in
      let nb = shift_m land ((1 lsl (bits)) - 1) in    (* 2^bits - 1 *)
      let c = String.make 1 (Char.chr nb) in
      (aux (n + 1)) ^ c
  in aux 0
