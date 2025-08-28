(* open Roberto_lib
open OUnit2

let test_scanner_tokens _ =
  let str = {| 
  print "Hello World"; 
  var variable = "string"
|} in
  let tokens = Scanner.scan_tokens str in
  let expected =
    [
      (Token.PRINT, 2, "print", None);
      ( Token.STRING,
        2,
        "\"Hello World\"",
        Some (Token.StringLiteral "Hello World") );
      (Token.SEMICOLON, 2, ";", None);
      (Token.VAR, 3, "var", None);
      (Token.IDENTIFIER, 3, "variable", None);
      (Token.EQUAL, 3, "=", None);
      (Token.STRING, 3, "\"string\"", Some (Token.StringLiteral "string"));
      (Token.EOF, 4, "", None);
    ]
  in
  assert_equal (List.length expected) (List.length tokens)
    ~msg:"Token count mismatch";

  List.iter2
    (fun (exp_type, exp_line, exp_lexeme, exp_literal) tok ->
      assert_equal exp_type tok.Token.token_type
        ~msg:(Printf.sprintf "Token type \n  mismatch for %s" exp_lexeme);
      assert_equal exp_line tok.Token.line
        ~msg:(Printf.sprintf "Line mismatch for \n  %s" exp_lexeme);
      assert_equal exp_lexeme tok.Token.lexeme
        ~msg:(Printf.sprintf "Lexeme mismatch");
      assert_equal exp_literal tok.Token.literal
        ~msg:(Printf.sprintf "Literal mismatch \n  for %s" exp_lexeme))
    expected tokens

let suite =
  "ScannerTests" >::: [ "test_scanner_tokens" >:: test_scanner_tokens ]

let () = OUnit2.run_test_tt_main suite *)
