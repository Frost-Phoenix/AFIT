(** Ciphers
    Builtin integer based ciphers.
*)

open Builtin
open Basic_arithmetics
open Power

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 256.
 *)
let encrypt_cesar k m b =
	let rec aux = function
		| [] -> []
		| e::q -> ((e+k) mod b)::(aux q)
	in aux m

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 256.
 *)
let decrypt_cesar k m b = encrypt_cesar (b - k) m b


(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q = 
	(* functions *)

	(* modulo *)
	let m = function
		| (a, n) when a < 0 -> n + (a mod n)
		| (a, n) -> a mod n in

	(* search for e so that 2 < e < phi and gcd(e, phi) == 1 *)
	let rec get_e phi = function
		| i when (gcd phi i) = 1 -> i
		|	i -> get_e phi (i+2) in

	(* search for d so that e*d mod phi == 1 (modulo inverse) *)
	let rec get_d phi e = 
		let rec get_mod_inverse x y u v = 
			let new_u = m ((x - (u * (x / u))), phi) 
			and new_v = m ((y - (v * (x / u))), phi) in
			match new_u with 
				| 1 -> new_v
				| _ -> get_mod_inverse u v new_u new_v
		in get_mod_inverse phi phi e 1 in
	
	(* Variables *)
	let n = p * q in
	let phi = (p-1) * (q-1) in
	let e = get_e phi 3 in
	let d = get_d phi e in
			
	(* public and private key *)
	((n,e), (n,d))

(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = mod_power m e n 

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = mod_power m d n


(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p = (0, 0)

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = (0, 0)

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA = (0, 0)

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = 0
