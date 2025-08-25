type tokenType =
  (* single character tokens *)
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  (* one or two character tokens *)
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  (* literals *)
  | IDENTIFIER
  | STRING
  | NUMBER
  (* keywords *)
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | EOF

type literal_value =
  | StringLiteral of string
  | NumberLiteral of float
  | BoolLiteral of bool
  | NilLiteral

type token = {
  token_type : tokenType;
  lexeme : string;
  literal : literal_value option;
  line : int;
}

let token_type_to_string (token_type : tokenType) =
  match token_type with
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | COMMA -> "COMMA"
  | DOT -> "DOT"
  | MINUS -> "MINUS"
  | PLUS -> "PLUS"
  | SEMICOLON -> "SEMICOLON"
  | SLASH -> "SLASH"
  | STAR -> "STAR"
  | BANG -> "BANG"
  | BANG_EQUAL -> "BANG_EQUAL"
  | EQUAL -> "EQUAL"
  | EQUAL_EQUAL -> "EQUAL_EQUAL"
  | GREATER -> "GREATER"
  | GREATER_EQUAL -> "GREATER_EQUAL"
  | LESS -> "LESS"
  | LESS_EQUAL -> "LESS_EQUAL"
  | IDENTIFIER -> "IDENTIFIER"
  | STRING -> "STRING"
  | NUMBER -> "NUMBER"
  | AND -> "AND"
  | CLASS -> "CLASS"
  | ELSE -> "ELSE"
  | FALSE -> "FALSE"
  | FUN -> "FUN"
  | FOR -> "FOR"
  | IF -> "IF"
  | NIL -> "NIL"
  | OR -> "OR"
  | PRINT -> "PRINT"
  | RETURN -> "RETURN"
  | SUPER -> "SUPER"
  | THIS -> "THIS"
  | TRUE -> "TRUE"
  | VAR -> "VAR"
  | WHILE -> "WHILE"
  | EOF -> "EOF"

let to_string token =
  let literal_str =
    match token.literal with
    | None -> ""
    | Some (StringLiteral s) -> " " ^ s
    | Some (NumberLiteral n) -> " " ^ string_of_float n
    | Some (BoolLiteral b) -> " " ^ string_of_bool b
    | Some NilLiteral -> " nil"
  in
  token_type_to_string token.token_type ^ " " ^ token.lexeme ^ literal_str
