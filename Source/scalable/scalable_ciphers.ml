(** Ciphers
    bitarrays based ciphers.
*)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(********** RSA Cipher **********)

(** Generate an RSA ciphering key.
    Involved prime bitarrays need to be distinct. Output is a couple
    of public, private keys.
    @param p prime bitarray
    @param q prime bitarray
*)
let generate_keys_rsa p q =
  (* functions *)

  (* search for e so that 2 < e < phi and gcd(e, phi) == 1 *)
  let rec get_e phi = function
    | i when (gcd_b phi i) = [0;1] -> i
    | i -> get_e phi (add_b i [0;0;1]) in

  (* search for d so that e*d mod phi == 1 (modulo inverse) *)
  let rec get_d phi e = 
    let rec get_mod_inverse x y u v = 
      let new_u = mod_b (diff_b x (mult_b u (quot_b x u))) phi 
      and new_v = mod_b (diff_b y (mult_b v (quot_b x u))) phi in
      match new_u with 
        | [0;1] -> new_v
        | _ -> get_mod_inverse u v new_u new_v
    in get_mod_inverse phi phi e [0;1] in
  
  (* Variables *)
  let n = mult_b p q in
  let phi = mult_b (diff_b p [0;1]) (diff_b q [0;1]) in
  let e = get_e phi [0;1;1] in
  let d = get_d phi e in
      
  (* public and private key *)
  ((n,e), (n,d))

(** Encryption using RSA cryptosystem.
    @param m bitarray hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = mod_power m e n 

(** Decryption using RSA cryptosystem.
    @param m bitarray hash of encrypted message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = mod_power m d n 

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is a prime bitarray and g a bitarray having high enough order modulo p.
    @param p is a prime bitarray having form 2*q + 1 for prime bitarray q.
 *)
let rec public_data_g p = ([], [])

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = ([], [])

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA = ([], [])

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = []
