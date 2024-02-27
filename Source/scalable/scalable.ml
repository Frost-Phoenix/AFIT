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
  
  (** Creates a natural bitarray (no sign bit) from a built-in integer.
      @param x built-in integer.
  *)
  let from_int_n x = match from_int x with
    | [] -> []
    | x::q -> q 
  
  (** Reverse 'a list
      @param  'a list
   *)
  let reverse l = 
    let rec aux l2 = function
      | [] -> l2
      | x::q -> aux (x::l2) q
    in aux [] l
  
  (** List len of 'a list
      @param 'a list
   *)
  let rec l_len = function
    | [] -> 0
    | x::q -> 1 + (l_len q)
  
  (** Creates a bitarray from a built-in integer.
      @param x built-in integer.
  *)
  let trim_0 l = 
    let rec aux = function
      | [] -> []
      | 1::q -> 1::q
      | 0::q -> aux q
      | _ -> failwith "trim_0: error unmatch case"
    in reverse (aux (reverse l))
  
        
  (** Transforms bitarray of built-in size to built-in integer.
      UNSAFE: possible integer overflow.
      @param bA bitarray object.
   *)
  let to_int bA = 
    let rec aux n = function
      | [] -> 0
      | x::q -> 
        match x with
          | 0 -> aux (n + 1) q
          | x -> (1 lsl n) + aux (n + 1) q        (* x == 1 *)
    in match bA with 
      | [] -> 0
      | 0::q -> aux 0 q
      | x::q -> (-1) * (aux 0 q)                  (* x == 1 *)
        
  
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
      | x::q -> print_string ("-" ^ (aux q))      (* x == 1 *)
  
  (** Toplevel directive to use print_b as bitarray printer.
      CAREFUL: print_b is then list int printer.
      UNCOMMENT FOR TOPLEVEL USE.
  *)
  (* #install_printer print_b *)
  
  (** Internal comparisons on bitarrays and naturals. Naturals in this
      context are understood as bitarrays missing a bit sign and thus
      assumed to be non-negative.
  *)
  
  
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
  let (<<!) nA nB = if compare_n nA nB = -1 then true else false
  
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
    if r = 1 then false
    else true
  
  
  (** Comparing two bitarrays. Output is 1 if first argument is bigger
      than second -1 otherwise.
      @param bA A bitarray.
      @param bB A bitarray.
  *)
  let compare_b bA bB = 
    match (bA, bB) with
      | ([], [])       ->  0
      | (0::q, [])     ->  1
      | (1::q, [])     -> -1
      | ([], 0::q)     -> -1
      | ([], 1::q)     ->  1
      | (0::q1, 1::q2) ->  1
      | (1::q1, 0::q2) -> -1
      | (1::q1, 1::q2) -> -(compare_n q1 q2)
      | (_::q1, _::q2) -> compare_n q1 q2
      | _ -> failwith "compare_b error"
      
  
  (** Bigger inorder comparison operator on bitarrays. Returns true if
      first argument is bigger than second and false otherwise.
      @param nA natural.
      @param nB natural.
   *)
  let (>>) bA bB = if compare_b bA bB = 1 then true else false
  
  (** Smaller inorder comparison operator on bitarrays. Returns true if
      first argument is smaller than second and false otherwise.
      @param nA natural.
      @param nB natural.
   *)
  let (<<) bA bB = if compare_b bA bB = -1 then true else false
  
  (** Bigger or equal inorder comparison operator on bitarrays. Returns
      true if first argument is bigger or equal to second and false
      otherwise.
      @param nA natural.
      @param nB natural.
   *)
  let (>>=) bA bB =
    let r = compare_b bA bB in
      if r = 1 || r = 0 then true
      else false
  
  (** Smaller or equal inorder comparison operator on naturals. Returns
      true if first argument is smaller or equal to second and false
      otherwise.
      @param nA natural.
      @param nB natural.
   *)
  let (<<=) bA bB =
    let r = compare_b bA bB in
      if r = 1 then false
      else true
  
  
  (** Sign of a bitarray.
      @param bA Bitarray.
  *)
  let sign_b bA = 
    match bA with 
      | []   ->  1
      | 0::_ ->  1
      | 1::_ -> -1
      | _ -> failwith "not valid bit array"
  
  
  (** Absolute value of bitarray.
      @param bA Bitarray.
  *)
  let abs_b bA = 
    match bA with
      | []   -> []
      | x::q -> 0::q
          
  
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
  let add_n nA nB =
    let rec aux = function
      | ([], [], 0) -> [ ]
      | ([], [], 1) -> [1]
      | (0::q1, [], 0) -> 0::(aux (q1, [], 0))
      | (0::q1, [], 1) -> 1::(aux (q1, [], 0))
      | (1::q1, [], 0) -> 1::(aux (q1, [], 0))
      | (1::q1, [], 1) -> 0::(aux (q1, [], 1))
      | ([], 0::q2, 0) -> 0::(aux ([], q2, 0))
      | ([], 0::q2, 1) -> 1::(aux ([], q2, 0))
      | ([], 1::q2, 0) -> 1::(aux ([], q2, 0))
      | ([], 1::q2, 1) -> 0::(aux ([], q2, 1))
      | (0::q1, 0::q2, 0) -> 0::(aux (q1, q2, 0))
      | (0::q1, 0::q2, 1) -> 1::(aux (q1, q2, 0))
      | (1::q1, 0::q2, 0) -> 1::(aux (q1, q2, 0))
      | (0::q1, 1::q2, 0) -> 1::(aux (q1, q2, 0))
      | (1::q1, 0::q2, 1) -> 0::(aux (q1, q2, 1))
      | (0::q1, 1::q2, 1) -> 0::(aux (q1, q2, 1))
      | (1::q1, 1::q2, 0) -> 0::(aux (q1, q2, 1))
      | (1::q1, 1::q2, 1) -> 1::(aux (q1, q2, 1))
      | _ -> failwith "add_n: error case unmatch"
    in aux (nA, nB, 0)
  
  
  
  (** Difference of two naturals.
      UNSAFE: First entry is assumed to be bigger than second.
      @param nA Natural.
      @param nB Natural.
  *)
  let diff_n nA nB = 
    let rec aux = function
      | ([], [], r) -> []
      | (b::q1, [], 0) -> b::(aux (q1, [], 0))
      | (0::q1, [], 1) -> 1::(aux (q1, [], 1))
      | (1::q1, [], 1) -> 0::(aux (q1, [], 0))
      | (0::q1, 0::q2, 0) -> 0::(aux (q1, q2, 0))
      | (0::q1, 0::q2, 1) -> 1::(aux (q1, q2, 1))
      | (1::q1, 0::q2, 0) -> 1::(aux (q1, q2, 0))
      | (1::q1, 0::q2, 1) -> 0::(aux (q1, q2, 0))
      | (0::q1, 1::q2, 0) -> 1::(aux (q1, q2, 1))
      | (0::q1, 1::q2, 1) -> 0::(aux (q1, q2, 1))
      | (1::q1, 1::q2, 0) -> 0::(aux (q1, q2, 0))
      | (1::q1, 1::q2, 1) -> 1::(aux (q1, q2, 1))
      | _ -> [55]
    in trim_0 (aux (nA, nB, 0))
  
  (** Addition of two bitarrays.
      @param bA Bitarray.
      @param bB Bitarray.
   *)
  let add_b bA bB = 
    let res = 
      match (bA, bB) with
        | [], [] -> []
        | [], l -> l
        | l, [] -> l
        | 0::q1, 0::q2 -> 0::(add_n q1 q2)
        | 1::q1, 1::q2 -> 0::(add_n q1 q2)
        | 0::q1, 1::q2 -> 
          if q1 >=! q2 then 0::(diff_n q1 q2)
          else 1::(diff_n q2 q1)
        | 1::q1, 0::q2 -> 
          if q2 >=! q1 then 0::(diff_n q1 q2)
          else 1::(diff_n q1 q2)
        | _ -> failwith "add_b: error unmatch case"
    in match res with
      | [0] -> []
      | l -> l
  
  (** Difference of two bitarrays.
      @param bA Bitarray.
      @param bB Bitarray.
  *)
  let diff_b bA bB = 
    let res = 
      match (bA, bB) with
        | [], [] -> []
        | l, [] -> l
        | [], 0::l -> 1::l
        | [], 1::l -> 0::l
        | 0::q1, 1::q2 -> 0::(add_n q1 q2)
        | 1::q1, 0::q2 -> 1::(add_n q1 q2)
        | 0::q1, 0::q2 -> 
          if q1 >=! q2 then 0::(diff_n q1 q2)
          else 1::(diff_n q2 q1)
        | 1::q1, 1::q2 -> 
          if q1 >>! q2 then 1::(diff_n q1 q2)
          else if q2 >>! q1 then 0::(diff_n q2 q1)
          else []
        | _ -> failwith "add_b: error unmatch case"
    in match res with
      | [0] -> []
      | l -> l
  
  (** Shifts natural to the left by a given natural number.
      @param nA natural.
      @param d Non-negative integer.
  *)
  let rec shift_n nA d = 
    match (d, nA) with
      | _, [] -> []
      | 0, l -> l
      | n, l -> 0::(shift_n l (n - 1))
  
  
  (** Shifts bitarray to the left by a given natural number.
      @param bA Bitarray.
      @param d Non-negative integer.
  *)
  let rec shift bA d = 
    match (bA, d) with
      | [], _ -> []
      | l, 0  -> l
      | b::q, n -> b::(shift_n q n)
  
  (** Shifts bitarray to the right by a given natural number.
      @param bA Bitarray.
      @param d Non-negative integer.
  *)
  let rec shift_r bA d = 
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
  
  (** Multiplication of two naturals.
      @param nA natural.
      @param nB natural.
  *)
  let mult_n nA nB = 
      let rec aux n1 = function
          | [], res, n -> res
          | 0::q2, res, n -> aux n1 (q2, res, (n+1))
          | 1::q2, res, n -> aux n1 (q2, (add_n res (shift_n n1 n)), (n+1))
          | _ -> failwith "mult_n: error unmatch case"
      in if nA >=! nB then aux nA (nB, [], 0)
      else aux nB (nA, [], 0)
  
  
  
  (** Multiplication of two bitarrays.
      @param bA Bitarray.
      @param bB Bitarray.
  *)
  let mult_b bA bB = 
      match (bA, bB) with
          | ([],_) | (_,[]) -> []
          | (x1::q1, x2::q2) when x1 = x2  -> 0::(mult_n q1 q2)
          | (x1::q1, x2::q2) when x1 != x2 -> 1::(mult_n q1 q2)
          | _ -> failwith "mult_b: error unmatch case"
      
  
  (** Quotient of two bitarrays.
      @param bA Bitarray you want to divide by second argument.
      @param bB Bitarray you divide by. Non-zero!
  *)
  (*
  let quot_n nA nB = 
      let rec aux res = function
          | (_, []) -> failwith "quot_n: error division by zero"
          | ([], _) -> res
          | (l1, l2) when l1 <<! l2 -> res
          | (l1, l2) -> aux (add_n res [1]) ((diff_n l1 l2), l2)
      in aux [] (nA, nB)
  *)
  let quot_n nA nB =
    let n = (l_len nA) - (l_len nB) in 
      let rec aux res = function
        | [], _, (-1) -> trim_0(res)
        | _, _, (-1) -> trim_0(res)
        | l1, l2, c -> 
            if (l1 >>= l2) then aux (1::res) ((diff_b l1 l2), (shift_r l2 1), (c-1))
            else aux (0::res) (l1, (shift_r l2 1), (c-1))
    in match nA, nB with 
      | [], _ -> []
      | nA, nB when nA <<! nB -> []
      | nA, nB -> aux [] ((0::0::nA), (0::(shift (0::nB) n)), n)
  
          
  (** Quotient of two bitarrays.
      @param bA Bitarray you want to divide by second argument.
      @param bB Bitarray you divide by. Non-zero!
  *)
  let quot_b bA bB = 
      match (bA, bB) with
          | _, [] -> failwith "quot_b: error division by zero"
          | [], _ -> []
          | (x1::q1, x2::q2) when x1 = x2  -> 
              let res = match (quot_n q1 q2) with
                                      | [] -> []
                                      | res -> 0::res
              in res
          | (x1::q1, x2::q2) when x1 != x2 -> 
                  let res = 1::(quot_n q1 q2) in 
                  if (mult_b res bB) = bA then res
                  else diff_b res (from_int 1)
          | _ -> failwith "quot_b: error unmatch case"
          
  
  (** Modulo of a bitarray against a positive one.
      @param bA Bitarray the modulo of which you're computing.
      @param bB Bitarray which is modular base.
   *)
  let mod_b bA bB = diff_b bA (mult_b bB (quot_b bA bB))
  
  (** Integer division of two bitarrays.
      @param bA Bitarray you want to divide.
      @param bB Bitarray you wnat to divide by.
  *)
  let div_b bA bB = (quot_b bA bB, mod_b bA bB)
  
  
  (** Logical and of two bitarrays.
      @param bA Bitarray.
      @param bB Bitarray.
  *)
  let and_b bA bB = 
      let rec aux = function 
          | [], _ -> []
          | _, [] -> []
          | x1::q1, x2::q2 -> (x1 land x2)::(aux (q1, q2))
      in match (bA, bB) with 
          | [], _ -> []
          | _, [] -> []
          | _::q1, _::q2 ->
              if q1 >>= q2 then 0::(aux (q1, q2))
              else 0::(aux (q2, q1))
  