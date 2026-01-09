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

let new_lexer (input : string) : lexer =
  read_char { input; position = 0; read_position = 0; ch = None }

let new_token (tt : Token.token_type) (ch : char option) : Token.token =
  {
    typ = tt;
    literal = Option.fold ~none:"" ~some:(fun c -> String.make 1 c) ch;
  }
