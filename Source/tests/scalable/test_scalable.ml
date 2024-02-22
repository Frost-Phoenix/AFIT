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
        check (list int) (print_bA "to_int: " bA) expected (reverse bA)
    in
    List.iter do_check cases


(****************************************************************************)
(****************************************************************************)

let scalable_set = [
	("From_int function", `Quick, from_int_tests);
	("To_int function",   `Quick, to_int_tests);
	("Reverse function",  `Quick, reverse_tests);
]
