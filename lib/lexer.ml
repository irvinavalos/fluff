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
  ("a" <= c && c <= "z") || ("A" <= c && c <= "Z") || c == "_"

let is_letter (ch : string) : bool = is_char_valid ch

let rec read_identifier_acc l acc =
  if is_letter (Option.fold ~none:"" ~some:(fun x -> String.make 1 x) l.ch) then
    read_identifier_acc (read_char l) (acc + 1)
  else acc

let read_identifer_helper l = read_identifier_acc l l.position

let read_identifier (l : lexer) : string =
  let old_position = l.position in
  let new_position = read_identifer_helper l in
  String.sub l.input old_position new_position

let new_lexer (input : string) : lexer =
  read_char { input; position = 0; read_position = 0; ch = None }

let new_token (tt : Token.token_type) (ch : string) : Token.token =
  { typ = tt; literal = ch }

let get_char_from_char_opt ch = Option.get ch

let next_token_helper l =
  let c = Option.fold ~none:"" ~some:(fun x -> String.make 1 x) l.ch in
  match c with
  | "{" -> new_token Token.LBRACE c
  | "}" -> new_token Token.RBRACE c
  | "(" -> new_token Token.LPAREN c
  | ")" -> new_token Token.RPAREN c
  | ";" -> new_token Token.SEMICOLON c
  | "," -> new_token Token.COMMA c
  | "+" -> new_token Token.PLUS c
  | "=" -> new_token Token.ASSIGN c
  | "" -> new_token Token.EOF c
  | _ ->
      if is_letter c then new_token (Token.lookup_ident c) (read_identifier l)
      else new_token Token.ILLEGAL c

let next_token (l : lexer) : lexer * Token.token =
  let return_token = next_token_helper l in
  let return_lexer = read_char l in
  (return_lexer, return_token)
