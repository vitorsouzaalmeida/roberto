open Token
open Sedlexing

let lexeme buf = Utf8.lexeme buf

let rec token buf =
  match%sedlex buf with
  | Plus (Chars " \r\t\n") -> token buf
  (* Single character tokens *)
  | '(' -> LEFT_PAREN
  | ')' -> RIGHT_PAREN
  | '{' -> LEFT_BRACE
  | '}' -> RIGHT_BRACE
  | ',' -> COMMA
  | '.' -> DOT
  | '-' -> MINUS
  | '+' -> PLUS
  | ';' -> SEMICOLON
  | '*' -> STAR
  (* Two character token *)
  | "!=" -> BANG_EQUAL
  | "==" -> EQUAL_EQUAL
  | "<=" -> LESS_EQUAL
  | ">=" -> GREATER_EQUAL
  (* Single or two character tokens *)
  | '!' -> BANG
  | '=' -> EQUAL
  | '<' -> LESS
  | '>' -> GREATER
  | '/' -> SLASH
  (* Literals *)
  | Plus '0' .. '9' -> NUMBER (* TODO: parse the number *)
  | '"', Star (Compl '"'), '"' -> STRING (* String literal *)
  | Plus ('a' .. 'z' | 'A' .. 'Z' | '_') -> (
      let lex = lexeme buf in
      match lex with
      | "and" -> AND
      | "class" -> CLASS
      | "else" -> ELSE
      | "false" -> FALSE
      | "for" -> FOR
      | "fun" -> FUN
      | "if" -> IF
      | "nil" -> NIL
      | "or" -> OR
      | "print" -> PRINT
      | "return" -> RETURN
      | "super" -> SUPER
      | "this" -> THIS
      | "true" -> TRUE
      | "var" -> VAR
      | "while" -> WHILE
      | _ -> IDENTIFIER)
  | eof -> EOF
  | _ -> failwith ("Unexpected character: " ^ lexeme buf)

let from_string str = Sedlexing.Utf8.from_string str

let scan_tokens str =
  let buf = from_string str in
  let rec loop acc =
    match token buf with
    | EOF -> List.rev (EOF :: acc)
    | tok -> loop (tok :: acc)
  in
  loop []
