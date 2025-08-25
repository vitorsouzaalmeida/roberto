open Roberto_lib
open OUnit2

let test_token _ =
  let str = {| print 1; |} in
  let all_tokens = Scanner.scan_tokens str in
  List.iter
    (fun tok -> Printf.printf "Token: %s\n" (Token.token_type_to_string tok))
    all_tokens

let suite = "ExampleTests" >::: [ "oloco" >:: test_token ]
let () = OUnit2.run_test_tt_main suite
