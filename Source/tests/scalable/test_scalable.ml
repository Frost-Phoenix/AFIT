(** Test suites for builtin basic_arithmetic ml file using alcotest. *)

open Alcotest
open Test_tools
open Scalable

let sprintf = Printf.sprintf

let rec print_bA fun_name bA = 
  let rec aux = function 
    | [] -> "]"
    | e::[] -> string_of_int e ^ "]" 
    | e::q -> string_of_int e ^ "; " ^ aux q
  in fun_name ^ "[" ^ aux bA

let from_int_tests () =
    let cases = [(0, []) ; ((-0), []) ; (42, [0;0;1;0;1;0;1]) ; ((-42), [1;0;1;0;1;0;1]) ; (127, [0;1;1;1;1;1;1;1])]
    and do_check (x, expected) =
        check (list int) (sprintf "from_int: %i" x) expected (from_int x)
    in
    List.iter do_check cases

let to_int_tests () =
    let cases = [([], 0) ; ([0;0;1;0;1;0;1], 42) ; ([1;0;1;0;1;0;1], (-42)) ; ([0;1;1;1;1;1;1;1], 127)]
    and do_check (bA, expected) =
        check int (print_bA "to_int: " bA) expected (to_int bA)
    in
    List.iter do_check cases

let reverse_tests () =
    let cases = [([], []) ; ([1], [1]) ; ([0;0;1;0;1;0;1], [1;0;1;0;1;0;0]) ; ([1;2;3;4;5], [5;4;3;2;1])]
    and do_check (bA, expected) =
        check (list int) (print_bA "reverse: " bA) expected (reverse bA)
    in
    List.iter do_check cases

let compare_n_tests () =
    let cases = [
      ((from_int_n 10, from_int_n 15), -1) ; ((from_int_n 15, from_int_n 10), 1) ; 
      ((from_int_n 0, from_int_n 15), -1) ; ((from_int_n 0, from_int_n 4), -1) ; 
      ((from_int_n 104243, from_int_n 15523523), -1) ; ((from_int_n 102142144, from_int_n 154324), 1) ;           
    ]
    and do_check ((nA, nB), expected) =
        check int (print_bA "compare_n: " nA ^ print_bA " ; " nB) expected (compare_n nA nB)
    in
    List.iter do_check cases

let bigger_strict_n_tests () =
    let cases = [
      ((from_int_n 10, from_int_n 15), false) ; ((from_int_n 15, from_int_n 10), true) ; 
      ((from_int_n 0, from_int_n 15), false) ; ((from_int_n 0, from_int_n 4), false) ; 
      ((from_int_n 104243, from_int_n 15523523), false) ; ((from_int_n 102142144, from_int_n 154324), true) ;           
    ]
    and do_check ((nA, nB), expected) =
        check bool (print_bA "bigger_stric_n: " nA ^ print_bA " ; " nB) expected (nA >>! nB)
    in
    List.iter do_check cases

let smaller_strict_n_tests () =
    let cases = [
      ((from_int_n 10, from_int_n 15), true) ; ((from_int_n 15, from_int_n 10), false) ; 
      ((from_int_n 0, from_int_n 15), true) ; ((from_int_n 0, from_int_n 4), true) ; 
      ((from_int_n 104243, from_int_n 15523523), true) ; ((from_int_n 102142144, from_int_n 154324), false) ;           
    ]
    and do_check ((nA, nB), expected) =
        check bool (print_bA "smaller_strict_n: " nA ^ print_bA " ; " nB) expected (nA <<! nB)
    in
    List.iter do_check cases


let bigger_n_tests () =
    let cases = [
      ((from_int_n 10, from_int_n 15), false) ; ((from_int_n 15, from_int_n 10), true) ; 
      ((from_int_n 0, from_int_n 15), false) ; ((from_int_n 0, from_int_n 4), false) ; 
      ((from_int_n 104243, from_int_n 15523523), false) ; ((from_int_n 102142144, from_int_n 154324), true) ;           
      ((from_int_n 10, from_int_n 10), true) ; ((from_int_n 0, from_int_n 0), true) ;           
    ]
    and do_check ((nA, nB), expected) =
        check bool (print_bA "bigger_b: " nA ^ print_bA " ; " nB) expected (nA >=! nB)
    in
    List.iter do_check cases

let smaller_n_tests () =
    let cases = [
      ((from_int_n 10, from_int_n 15), true) ; ((from_int_n 15, from_int_n 10), false) ; 
      ((from_int_n 0, from_int_n 15), true) ; ((from_int_n 0, from_int_n 4), true) ; 
      ((from_int_n 104243, from_int_n 15523523), true) ; ((from_int_n 102142144, from_int_n 154324), false) ;           
      ((from_int_n 10, from_int_n 10), true) ; ((from_int_n 0, from_int_n 0), true) ;           
    ]
    and do_check ((nA, nB), expected) =
        check bool (print_bA "smaller_n: " nA ^ print_bA " ; " nB) expected (nA <=! nB)
    in
    List.iter do_check cases

let compare_b_tests () =
    let cases = [
      ((from_int 10, from_int 15), -1) ; ((from_int 15, from_int 10), 1) ; 
      ((from_int 0, from_int 15), -1) ; ((from_int 0, from_int 4), -1) ; 
      ((from_int 104243, from_int 15523523), -1) ; ((from_int 102142144, from_int 154324), 1) ;           
      ((from_int (-10), from_int 15), -1) ; ((from_int (-15), from_int 10), -1) ; 
      ((from_int (0), from_int (-15)), 1) ; ((from_int (-15), from_int 10), -1) ; 
    ]
    and do_check ((bA, bB), expected) =
        check int (print_bA "compare_b: " bA ^ print_bA " ; " bB) expected (compare_b bA bB)
    in
    List.iter do_check cases

let bigger_strict_b_tests () =
    let cases = [
      ((from_int 10, from_int 15), false) ; ((from_int 15, from_int 10), true) ; 
      ((from_int 0, from_int 15), false) ; ((from_int 0, from_int 4), false) ; 
      ((from_int 104243, from_int 15523523), false) ; ((from_int 102142144, from_int 154324), true) ;           
    ]
    and do_check ((bA, bB), expected) =
        check bool (print_bA "bigger_stric_b: " bA ^ print_bA " ; " bB) expected (bA >> bB)
    in
    List.iter do_check cases

let smaller_strict_b_tests () =
    let cases = [
      ((from_int 10, from_int 15), true) ; ((from_int 15, from_int 10), false) ; 
      ((from_int 0, from_int 15), true) ; ((from_int 0, from_int 4), true) ; 
      ((from_int 104243, from_int 15523523), true) ; ((from_int 102142144, from_int 154324), false) ;           
      ((from_int (-10), from_int 15), true) ; ((from_int (-15), from_int 10), true) ; 
      ((from_int 0, from_int (-15)), false) ; ((from_int (-10), from_int (-15)), false) ;
      (([0; 1; 0; 0; 0; 1], [0; 1; 0; 0; 0; 1]), false) 
    ]
    and do_check ((bA, bB), expected) =
        check bool (print_bA "smaller_strict_b: " bA ^ print_bA " ; " bB) expected (bA << bB)
    in
    List.iter do_check cases


let bigger_b_tests () =
    let cases = [
      ((from_int 10, from_int 15), false) ; ((from_int 15, from_int 10), true) ; 
      ((from_int 0, from_int 15), false) ; ((from_int 0, from_int 4), false) ; 
      ((from_int 104243, from_int 15523523), false) ; ((from_int 102142144, from_int 154324), true) ;           
      ((from_int 10, from_int 10), true) ; ((from_int 0, from_int 0), true) ;           
    ]
    and do_check ((bA, bB), expected) =
        check bool (print_bA "bigger_b: " bA ^ print_bA " ; " bB) expected (bA >>= bB)
    in
    List.iter do_check cases

let smaller_b_tests () =
    let cases = [
      ((from_int 10, from_int 15), true) ; ((from_int 15, from_int 10), false) ; 
      ((from_int 0, from_int 15), true) ; ((from_int 0, from_int 4), true) ; 
      ((from_int 104243, from_int 15523523), true) ; ((from_int 102142144, from_int 154324), false) ;           
      ((from_int 10, from_int 10), true) ; ((from_int 0, from_int 0), true) ;           
    ]
    and do_check ((bA, bB), expected) =
        check bool (print_bA "smaller_b: " bA ^ print_bA " ; " bB) expected (bA <<= bB)
    in
    List.iter do_check cases

let sign_b_tests () =
    let cases = [
      ([], 1) ; (from_int 1, 1) ; (from_int (-1), -1) ; (from_int 42, 1) ; (from_int (-42), -1) 
    ]
    and do_check (bA, expected) =
        check int (print_bA "sign_b: " bA) expected (sign_b bA)
    in
    List.iter do_check cases

let abs_b_tests () =
    let cases = [
      ([], []) ; (from_int 1, from_int 1) ; (from_int (-1), from_int 1) ; 
      (from_int 42, from_int 42) ; (from_int (-42), from_int 42) 
    ]
    and do_check (bA, expected) =
        check (list int) (print_bA "abs_b: " bA) expected (abs_b bA)
    in
    List.iter do_check cases

let add_n_tests () =
    let cases = [
      ((from_int_n 10, from_int_n 15), from_int_n 25) ; ((from_int_n 15, from_int_n 10), from_int_n 25) ;
      ((from_int_n 18, from_int_n 13), from_int_n 31) ; ((from_int_n 23, from_int_n 11), from_int_n 34) ;
      ((from_int_n 85794, from_int_n 1532), from_int_n 87326) ; 
      ((from_int_n 2342134141, from_int_n 214654), from_int_n 2342348795) ;
    ]
    and do_check ((nA, nB), expected) =
        check (list int) (print_bA "add_n_tests: " nA ^ print_bA " ; " nB) expected (add_n nA nB)
    in
    List.iter do_check cases

let diff_n_tests () =
    let cases = [
      ((from_int_n 15, from_int_n 10), from_int_n 5) ; ((from_int_n 18, from_int_n 13), from_int_n 5) ; 
      ((from_int_n 23, from_int_n 11), from_int_n 12) ; ((from_int_n 85794, from_int_n 1532), from_int_n 84262) ; 
      ((from_int_n 2342134141, from_int_n 214654), from_int_n 2341919487) 
    ]
    and do_check ((nA, nB), expected) =
        check (list int) (print_bA "diff_n_tests: " nA ^ print_bA " ; " nB) expected (diff_n nA nB)
    in
    List.iter do_check cases

let add_b_tests () =
    let cases = [
      ((from_int (-20), from_int 20), from_int 0) ;
      ((from_int 10, from_int 15), from_int 25) ; ((from_int 15, from_int 10), from_int 25) ;
      ((from_int 18, from_int 13), from_int 31) ; ((from_int 23, from_int 11), from_int 34) ;
      ((from_int 85794, from_int 1532), from_int 87326) ; 
      ((from_int 2342134141, from_int 214654), from_int 2342348795) ;
      ((from_int 10, from_int (-15)), from_int (-5)) ; ((from_int (-15), from_int 10), from_int (-5)) ;
      ((from_int 18, from_int (-13)), from_int 5) ; ((from_int (-23), from_int 11), from_int (-12)) ;
      ((from_int (-18), from_int (-13)), from_int (31)) ; ((from_int (-23), from_int (-11)), from_int (34)) ;
      ((from_int 85794, from_int 1532), from_int 87326) ; 
      ((from_int 2342134141, from_int 214654), from_int 2342348795) ;
      ((from_int (-85794), from_int 1532), from_int (-84262)) ; 
      ((from_int (-2342134141), from_int (-214654)), from_int (2342348795)) ;
    ]
    and do_check ((bA, bB), expected) =
        check (list int) (print_bA "add_n_tests: " bA ^ print_bA " ; " bB) expected (add_b bA bB)
    in
    List.iter do_check cases

let diff_b_tests () =
    let cases = [
      ((from_int 7, from_int 7), from_int 0) ;
      ((from_int (-7), from_int (-7)), from_int 0) ;
      ((from_int (-20), from_int 20), from_int (-40)) ;
      ((from_int (-21), from_int (-20)), from_int (-1)) ;
      ((from_int (-20), from_int (-21)), from_int 1) ;
      ((from_int (-20), from_int (-20)), from_int 0) ;
      ((from_int (-20), from_int 20), from_int (-40)) ;
      ((from_int 20, from_int (-20)), from_int 40) ;
      ((from_int 20, from_int 20), from_int 0) ;
      ((from_int 10, from_int 15), from_int (-5)) ; ((from_int 15, from_int 10), from_int 5) ;
      ((from_int 18, from_int 13), from_int 5) ; ((from_int 23, from_int 11), from_int 12) ;
      ((from_int 85794, from_int 1532), from_int 84262) ; 
      ((from_int 2342134141, from_int 214654), from_int 2341919487) ;
      ((from_int 10, from_int (-15)), from_int 25) ; ((from_int (-15), from_int 10), from_int (-25)) ;
      ((from_int 18, from_int (-13)), from_int 31) ; ((from_int (-23), from_int 11), from_int (-34)) ;
      ((from_int (-18), from_int (-13)), from_int (-5)) ; ((from_int (-23), from_int (-11)), from_int (-12)) ;
      ((from_int 85794, from_int (-1532)), from_int 87326) ; 
      ((from_int (-2342134141), from_int (-214654)), from_int (-2341919487)) ;
      ((from_int (-85794), from_int 1532), from_int (-87326)) ; 
      ((from_int (-2342134141), from_int (-214654)), from_int (-2341919487)) ;
    ]
    and do_check ((bA, bB), expected) =
        check (list int) (print_bA "diff_n_tests: " bA ^ print_bA " ; " bB) expected (diff_b bA bB)
    in
    List.iter do_check cases

let shift_n_tests () =
    let cases = [
      ((from_int_n 1, 10), from_int_n 1024) ; ((from_int_n 1, 0), from_int_n 1) ;
      ((from_int_n 123, 24), from_int_n 2063597568) ; ((from_int_n 1, 0), from_int_n 1) ;
      ((from_int_n 0, 1), from_int_n 0) ; ((from_int_n 10, 0), from_int_n 10) ;
      ((from_int_n 10, 1), from_int_n 20) ; ((from_int_n 10, 2), from_int_n 40) ;
    ]
    and do_check ((nA, n), expected) =
        check (list int) (print_bA "shift_n: " nA ^ " ; n = " ^ string_of_int n) expected (shift_n nA n)
    in
    List.iter do_check cases

let shift_tests () =
    let cases = [
      ((from_int 1, 10), from_int 1024) ; ((from_int 1, 0), from_int 1) ;
      ((from_int 123, 24), from_int 2063597568) ; ((from_int 3, 7), from_int 384) ;
      ((from_int 0, 1), from_int 0) ; ((from_int 10, 0), from_int 10) ;
      ((from_int 10, 1), from_int 20) ; ((from_int 10, 2), from_int 40) ;
      ((from_int (-10), 1), from_int (-20)) ; ((from_int (-10), 2), from_int (-40)) ;
    ]
    and do_check ((bA, n), expected) =
        check (list int) (print_bA "shift_b: " bA ^ " ; n = " ^ string_of_int n) expected (shift bA n)
    in
    List.iter do_check cases

let shift_r_tests () =
    let cases = [
      ((from_int 1, 10), from_int 0) ; ((from_int 1, 0), from_int 1) ;
      ((from_int 123, 2), from_int 30) ; ((from_int 123, 0), from_int 123) ;
      ((from_int 0, 1), from_int 0) ; ((from_int 10, 0), from_int 10) ;
    ]
    and do_check ((bA, n), expected) =
        check (list int) (print_bA "shift_r: " bA ^ " ; n = " ^ string_of_int n) expected (shift_r bA n)
    in
    List.iter do_check cases

let mult_n_tests () =
    let cases = [
			((from_int_n 13, from_int_n 57), from_int_n 741) ; 
			((from_int_n 0, from_int_n 57), from_int_n 0) ; 
			((from_int_n 13, from_int_n 0), from_int_n 0) ; 
			((from_int_n 0, from_int_n 0), from_int_n 0) ; 
			((from_int_n 38753, from_int_n 4334985), from_int_n 167993673705) ; 
    ]
    and do_check ((nA, nB), expected) =
        check (list int) (print_bA "mult_n_tests: " nA ^ print_bA " ; " nB) expected (mult_n nA nB)
    in
    List.iter do_check cases

let mult_b_tests () =
    let cases = [
			((from_int 13, from_int 57), from_int 741) ; 
			((from_int 0, from_int 57), from_int 0) ; 
			((from_int 13, from_int 0), from_int 0) ; 
			((from_int 0, from_int 0), from_int 0) ; 
			((from_int 38753, from_int 4334985), from_int 167993673705) ; 
			((from_int (-13), from_int (-57)), from_int 741) ; 
			((from_int (-0), from_int (-57)), from_int 0) ; 
			((from_int (-38753), from_int (-4334985)), from_int 167993673705) ; 
			((from_int (-13), from_int 57), from_int (-741)) ; 
			((from_int 0, from_int (-57)), from_int 0) ; 
			((from_int 38753, from_int (-4334985)), from_int (-167993673705)) ; 
    ]
    and do_check ((bA, bB), expected) =
        check (list int) (print_bA "mult_b_tests: " bA ^ print_bA " ; " bB) expected (mult_b bA bB)
    in
    List.iter do_check cases

let quot_n_tests () =
    let cases = [
			((from_int_n 13, from_int_n 5), from_int_n 2) ; 
			((from_int_n 27, from_int_n 3), from_int_n 9) ; 
			((from_int_n 1245, from_int_n 5), from_int_n 249) ; 
			((from_int_n 2093858594, from_int_n 23525), from_int_n 89005) ; 
	    ((from_int_n 239859843, from_int_n 2323), from_int_n 103254) ; 
	 	  ((from_int_n 98598435, from_int_n 2323), from_int_n 42444) ; 

    ]
    and do_check ((nA, nB), expected) =
        check (list int) (print_bA "quot_n_tests: " nA ^ print_bA " ; " nB) expected (quot_n nA nB)
    in
    List.iter do_check cases

let quot_b_tests () =
    let cases = [
			((from_int 1, from_int 2), from_int 0) ; 
			((from_int 10, from_int 3), from_int 3) ; 
			((from_int 13, from_int 5), from_int 2) ; 
			((from_int 27, from_int 3), from_int 9) ; 
			((from_int 1245, from_int 5), from_int 249) ; 
			((from_int 2093858594, from_int 23525), from_int 89005) ; 
	    ((from_int 98598435, from_int 2323), from_int 42444) ; 
			((from_int (-10), from_int 3), from_int (-4)) ; 
			((from_int (-10), from_int 2), from_int (-5)) ; 
			((from_int 10, from_int (-3)), from_int (-4)) ; 
			((from_int 10, from_int (-2)), from_int (-5)) ; 
			((from_int (-10), from_int (-3)), from_int 3) ; 
			((from_int (-10), from_int (-2)), from_int 5) ; 
    ]
    and do_check ((bA, bB), expected) =
        check (list int) (print_bA "quot_b_tests: " bA ^ print_bA " ; " bB) expected (quot_b bA bB)
    in
    List.iter do_check cases

let mod_b_tests () =
    let cases = [
			((from_int 10, from_int 3), from_int 1) ; 
			((from_int 13, from_int 5), from_int 3) ; 
			((from_int 27, from_int 3), from_int 0) ; 
			((from_int 1245, from_int 5), from_int 0) ; 
			((from_int 321245432, from_int 43), from_int 0) ; 
			((from_int (-10), from_int 3), from_int 2) ; 
			((from_int (-10), from_int 2), from_int 0) ; 
    ]
    and do_check ((bA, bB), expected) =
        check (list int) (print_bA "mod_b_tests: " bA ^ print_bA " ; " bB) expected (mod_b bA bB)
    in
    List.iter do_check cases

let and_b_tests () =
    let cases = [
			((from_int 10, from_int 3), from_int 2) ; 
			((from_int 13, from_int 5), from_int 5) ; 
			((from_int 27, from_int 3), from_int 3) ; 
			((from_int 1245, from_int 5), from_int 5) ; 
			((from_int 11, from_int 10), from_int 10) ; 
    ]
    and do_check ((bA, bB), expected) =
        check (list int) (print_bA "and_b_tests: " bA ^ print_bA " ; " bB) expected (and_b bA bB)
    in
    List.iter do_check cases



(****************************************************************************)
(****************************************************************************)

let scalable_set = [
  ("From_int function", `Quick, from_int_tests);
  ("To_int function", `Quick, to_int_tests);

  ("Reverse function", `Quick, reverse_tests);

  ("Compare n function", `Quick, compare_n_tests);
  ("Bigger strict n function", `Quick, bigger_strict_n_tests);
  ("Smaller strict n function", `Quick, smaller_strict_n_tests);
  ("Bigger n function", `Quick, bigger_n_tests);
  ("Smaller n function", `Quick, smaller_n_tests);

  ("Compare b function", `Quick, compare_b_tests);
  ("Bigger strict b function", `Quick, bigger_strict_b_tests);
  ("Smaller strict b function", `Quick, smaller_strict_b_tests);
  ("Bigger b function", `Quick, bigger_b_tests);
  ("Smaller b function", `Quick, smaller_b_tests);

  ("Sign b function", `Quick, sign_b_tests);
  ("Abs b function", `Quick, abs_b_tests);

  ("Add n function", `Quick, add_n_tests);
  ("Diff n function", `Quick, diff_n_tests);

  ("Add b function", `Quick, add_b_tests); 
  ("Diff b function", `Quick, diff_b_tests); 

  ("Shift n function", `Quick, shift_n_tests); 
  ("Shift b function", `Quick, shift_tests); 
  ("Shift_r b function", `Quick, shift_r_tests); 

  ("Mult n function", `Quick, mult_n_tests); 
  ("Mult b function", `Quick, mult_b_tests); 

  ("Quot n function", `Quick, quot_n_tests); 
  ("Quot b function", `Quick, quot_b_tests); 
  ("Mod b function", `Quick, mod_b_tests); 

  ("And b function", `Quick, and_b_tests); 
]
