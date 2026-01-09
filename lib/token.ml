type token_type =
  | ILLEGAL
  | EOF
  | IDENT
  | INT
  | ASSIGN
  | PLUS
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | FUNCTION
  | VAL

type token = { typ : token_type; literal : string }

let string_of_token_type (tt : token_type) : string =
  match tt with
  | ILLEGAL -> "ILLEGAL"
  | EOF -> "EOF"
  (* Identifies and Literals *)
  | IDENT -> "IDENT"
  | INT -> "INT"
  (* Operators *)
  | ASSIGN -> "ASSIGN"
  | PLUS -> "PLUS"
  (* Delimiters *)
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  (* Keywords *)
  | FUNCTION -> "FUNCTION"
  | VAL -> "VAL"
