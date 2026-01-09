type lexer = {
  input : string;
  position : int; (* pointer to current position (current character) *)
  read_position : int; (* current reading position (after position) *)
  ch : char option; (* current character that is being evaluated *)
}

let read_char (l : lexer) : lexer =
  let ch =
    if l.read_position >= String.length l.input then None
    else Some l.input.[l.read_position]
  in
  {
    input = l.input;
    position = l.read_position;
    read_position = l.read_position + 1;
    ch;
  }

let is_char_valid c =
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_'

let is_letter (ch : char option) : bool =
  Option.fold ~none:false ~some:(fun c -> is_char_valid c) ch

let rec read_identifier_acc l acc =
  if is_letter l.ch then read_identifier_acc (read_char l) (acc + 1) else acc

let read_identifer_helper l = read_identifier_acc l l.position

let read_identifier (l : lexer) : lexer * string =
  let old_position = l.position in
  let new_position = read_identifer_helper l in
  ( {
      input = l.input;
      position = new_position;
      read_position = new_position + 1;
      ch = Some l.input.[new_position];
    },
    String.sub l.input old_position new_position )

let new_lexer (input : string) : lexer =
  read_char { input; position = 0; read_position = 0; ch = None }

let new_token (tt : Token.token_type) (ch : char option) : Token.token =
  {
    typ = tt;
    literal = Option.fold ~none:"" ~some:(fun c -> String.make 1 c) ch;
  }

let get_char_from_char_opt ch = Option.get ch

let next_token_helper ch =
  let tt =
    let c = get_char_from_char_opt ch in
    match c with
    | '{' -> Token.LBRACE
    | '}' -> Token.RBRACE
    | '(' -> Token.LPAREN
    | ')' -> Token.RPAREN
    | ';' -> Token.SEMICOLON
    | ',' -> Token.COMMA
    | '+' -> Token.PLUS
    | '=' -> Token.ASSIGN
    | _ -> Token.EOF
  in
  new_token tt ch

let next_token (l : lexer) : lexer * Token.token =
  let return_token = next_token_helper l.ch in
  let return_lexer = read_char l in
  (return_lexer, return_token)
