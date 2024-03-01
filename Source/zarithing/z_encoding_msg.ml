(** Encoding Strings *)

open Z
open Z_power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits = 
  let str_len = of_int (String.length str) in
  let b = of_int bits in
  let rec aux = function 
    | i when i = zero -> zero
    | i -> 
      let c = str.[to_int (str_len - i)] in
      let nb = of_int (Char.code c) in                   (* ascii code of c *)
      ((shift_left nb (to_int((i - one) * b)))) + aux (i-one)     (* put ascii code of c to the left of the result *)
  in aux str_len

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let decode msg bits = 
  let b = of_int bits in
  let rec aux = function 
    | n when shift_right msg (to_int(n * b)) = zero -> ""
    | n -> 
      let shift_m = (shift_right msg (to_int(n * b))) in
      let nb = shift_m land ((one lsl bits) - one) in    (* 2^bits - 1 *)
      let c = String.make 1 (Char.chr (to_int nb)) in
      (aux (n + one)) ^ c
  in aux zero

