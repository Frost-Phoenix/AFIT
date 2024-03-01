(** Testing for primality *)

open Z
open Z_power

(** Deterministic primality test
    @param n a big integer bigger or equal to 2.
 *)
let is_prime n = 
  let rec aux d = d * d > n || (n mod d <> zero && aux (d + (of_int 2)))
  in n = (of_int 2) || (n > (of_int 2) && (n mod (of_int 2) <> zero && aux (of_int 3)))
