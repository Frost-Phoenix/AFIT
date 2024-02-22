(** Test suites for builtin encoding_msg ml file using alcotest. *)

open Alcotest
open Encoding_msg

let sprintf = Printf.sprintf

let encode_tests () =
    (* 2294023860466 => 1000010 / 1100001 / 1110011 / 1101000 / 1100001 / 1110010 *)
    let cases = [ (("A", 7), 65); (("AB", 7), 8386); (("Bashar", 7), 2294023860466)]
    and do_check ((str, bits), expected) =
        check int (sprintf "encode: \"%s\" on %i bits" str bits) expected (encode str bits)
    in
    List.iter do_check cases

let decode_tests () =
    let cases = [((2294023860466, 7), "Bashar"); ((2379608033576814706, 11), "Bashar")]
    and do_check ((msg, bits), expected) =
        check string (sprintf "decode: %i on %i bits" msg bits) expected (decode msg bits)
    in
    List.iter do_check cases

(****************************************************************************)
(****************************************************************************)

let encoding_msg_set =
    [("Encode function", `Quick, encode_tests);
     ("Decode function", `Quick, decode_tests)]
