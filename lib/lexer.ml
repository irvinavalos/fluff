type lexer = {
  input : string;
  position : int; (* pointer to current position (current character) *)
  read_position : int; (* current reading position (after position) *)
  ch : char option; (* current character that is being evaluated *)
}

let read_char (l : lexer) : lexer =
  match l.read_position with
  | pos when pos >= 0 && pos <= String.length l.input - 1 ->
      {
        input = l.input;
        position = l.read_position;
        read_position = l.read_position + 1;
        ch = Some l.input.[l.read_position];
      }
  | _ ->
      {
        input = l.input;
        position = l.read_position;
        read_position = l.read_position + 1;
        ch = None;
      }

let new_lexer (input : string) : lexer =
  read_char { input; position = 0; read_position = 0; ch = None }
