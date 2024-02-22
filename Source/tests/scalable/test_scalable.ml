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
        check bool (print_bA "bigger_n: " nA ^ print_bA " ; " nB) expected (nA >=! nB)
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
      ((from_int_n 10, from_int_n 15), -1) ; ((from_int_n 15, from_int_n 10), 1) ; 
      ((from_int_n 0, from_int_n 15), -1) ; ((from_int_n 0, from_int_n 4), -1) ; 
      ((from_int_n 104243, from_int_n 15523523), -1) ; ((from_int_n 102142144, from_int_n 154324), 1) ;           
    ]
    and do_check ((nA, nB), expected) =
        check int (print_bA "compare_b: " nA ^ print_bA " ; " nB) expected (compare_b nA nB)
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

]
