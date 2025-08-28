(* open Token *)
open Sedlexing
open Parser

let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit, Opt ('.', Plus digit)]
let lexeme buf = Utf8.lexeme buf

type scanner_state = { mutable line : int }

let state = { line = 1 }

exception Eof

(* let create_token_record token_type lexeme_str literal =
  { token_type; lexeme = lexeme_str; literal; line = state.line } *)

let rec token buf =
  match%sedlex buf with
  | Plus (Chars " \r\t\n") -> token buf
  | '+' -> PLUS
  | '(' -> LEFT_PAREN
  | ')' -> RIGHT_PAREN
  (* | Plus ('a' .. 'z' | 'A' .. 'Z' | '_') -> (
      let lex = lexeme buf in
      match lex with
      | "print" -> PRINT
      | _ -> failwith ("Unknown identifier: " ^ lex)) *)
  | eof -> EOF
  | number -> NUMBER (float_of_string (Sedlexing.Utf8.lexeme buf))
  | _ -> failwith "Unexpected character"

(* let rec token buf =
  match%sedlex buf with
  | Plus (Chars " \r\t") -> token buf
  | '\n' ->
      state.line <- state.line + 1;
      token buf
  (* Single character tokens *)
  | '(' -> create_token_record LEFT_PAREN (lexeme buf) None
  | ')' -> create_token_record RIGHT_PAREN (lexeme buf) None
  | '{' -> create_token_record LEFT_BRACE (lexeme buf) None
  | '}' -> create_token_record RIGHT_BRACE (lexeme buf) None
  | ',' -> create_token_record COMMA (lexeme buf) None
  | '.' -> create_token_record DOT (lexeme buf) None
  | '-' -> create_token_record MINUS (lexeme buf) None
  | '+' -> create_token_record PLUS (lexeme buf) None
  | ';' -> create_token_record SEMICOLON (lexeme buf) None
  | '*' -> create_token_record STAR (lexeme buf) None
  (* Two character token *)
  | "!=" -> create_token_record BANG_EQUAL (lexeme buf) None
  | "==" -> create_token_record EQUAL_EQUAL (lexeme buf) None
  | "<=" -> create_token_record LESS_EQUAL (lexeme buf) None
  | ">=" -> create_token_record GREATER_EQUAL (lexeme buf) None
  (* Single or two character tokens *)
  | '!' -> create_token_record BANG (lexeme buf) None
  | '=' -> create_token_record EQUAL (lexeme buf) None
  | '<' -> create_token_record LESS (lexeme buf) None
  | '>' -> create_token_record GREATER (lexeme buf) None
  | '/' -> create_token_record SLASH (lexeme buf) None
  (* Literals *)
  | Plus '0' .. '9', Opt ('.', Plus '0' .. '9') ->
      let lex = lexeme buf in
      let num = float_of_string lex in
      create_token_record NUMBER lex (Some (NumberLiteral num))
  | '"', Star (Compl '"'), '"' ->
      let lex = lexeme buf in
      let str_content = String.sub lex 1 (String.length lex - 2) in
      create_token_record STRING lex (Some (StringLiteral str_content))
  | Plus ('a' .. 'z' | 'A' .. 'Z' | '_') ->
      let lex = lexeme buf in
      let token_type, literal =
        match lex with
        | "and" -> (AND, None)
        | "class" -> (CLASS, None)
        | "else" -> (ELSE, None)
        | "false" -> (FALSE, Some (BoolLiteral false))
        | "for" -> (FOR, None)
        | "fun" -> (FUN, None)
        | "if" -> (IF, None)
        | "nil" -> (NIL, Some NilLiteral)
        | "or" -> (OR, None)
        | "print" -> (PRINT, None)
        | "return" -> (RETURN, None)
        | "super" -> (SUPER, None)
        | "this" -> (THIS, None)
        | "true" -> (TRUE, Some (BoolLiteral true))
        | "var" -> (VAR, None)
        | "while" -> (WHILE, None)
        | _ -> (IDENTIFIER, None)
      in
      create_token_record token_type lex literal
  | eof -> create_token_record EOF "" None
  | _ -> failwith ("Unexpected character: " ^ lexeme buf) *)

(* let from_string str = Sedlexing.Utf8.from_string str

let scan_tokens str =
  state.line <- 1;
  let buf = from_string str in
  let rec loop acc =
    let tok = token buf in
    match tok.token_type with
    | EOF -> List.rev (tok :: acc)
    | _ -> loop (tok :: acc)
  in
  loop [] *)
