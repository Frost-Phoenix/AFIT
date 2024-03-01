(** Ciphers
    Big integers based ciphers.
*)

open Z
open Z_power

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q =
  let rec get_e phi = function
    | i when (gcd phi i) = one -> i
    | i -> get_e phi (i + (of_int 2)) in
  (* Variables *)
  let n = p * q in
  let phi = (p - (of_int 1)) * (q - (of_int 1)) in
  let e = get_e phi (of_int 3) in
  let d = invert e phi 
  in ((n, e), (n, d))

(** Encryption using RSA cryptosystem.
    @param m big integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = powm m e n

(** Decryption using RSA cryptosystem.
    @param m big integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = powm m d n

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p = 
  match of_int (Random.int (to_int p)) with
		| q when q*q mod p <> one -> (p,q)
		| _ -> public_data_g p
	

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = 
  let a = of_int (Random.int (to_int p)) in
	let public_key = powm g a p
	in (public_key, a)

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA = 
  let k = of_int (Random.int (to_int p)) in
	let c1 = powm g k p in
	let c2 = msg * (powm kA k p)
	in (c1, c2)

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = msgB / (powm msgA a p) mod p
