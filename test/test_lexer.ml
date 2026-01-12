open Alcotest

let pp_token_type fmt t =
  Format.pp_print_string fmt (Fluff.Token.string_of_token_type t)

let token_type_testable = Alcotest.testable pp_token_type ( = )

type test_case = {
  expected_type : Fluff.Token.token_type;
  expected_literal : string;
}

let test_lexer_simple () =
  let input = "=+(){},;" in

  let tests =
    [
      { expected_type = ASSIGN; expected_literal = "=" };
      { expected_type = PLUS; expected_literal = "+" };
      { expected_type = LPAREN; expected_literal = "(" };
      { expected_type = RPAREN; expected_literal = ")" };
      { expected_type = LBRACE; expected_literal = "{" };
      { expected_type = RBRACE; expected_literal = "}" };
      { expected_type = COMMA; expected_literal = "," };
      { expected_type = SEMICOLON; expected_literal = ";" };
    ]
  in

  let l = Fluff.Lexer.new_lexer input in

  List.iteri
    (fun i tt ->
      let _, tok = Fluff.Lexer.next_token l in

      Alcotest.check string
        ("token literal " ^ string_of_int i)
        tt.expected_literal
        (Fluff.Token.get_token_literal tok);

      Alcotest.check token_type_testable
        ("token type " ^ string_of_int i)
        tt.expected_type
        (Fluff.Token.get_token_type tok))
    tests

let test_next_token () =
  let input =
    "val five = 5;\n\n\n\
     val ten = 10;\n\n\n\n\
     val add = fn(x, y) {\n\n\n\
     \tx + y;\n\n\
     };\n\n\n\n\
     val res = add(five, ten);"
  in

  let tests =
    [
      { expected_type = VAL; expected_literal = "val" };
      { expected_type = ASSIGN; expected_literal = "=" };
      { expected_type = IDENT; expected_literal = "five" };
      { expected_type = INT; expected_literal = "5" };
      { expected_type = SEMICOLON; expected_literal = ";" };
      { expected_type = VAL; expected_literal = "val" };
      { expected_type = IDENT; expected_literal = "ten" };
      { expected_type = ASSIGN; expected_literal = "=" };
      { expected_type = INT; expected_literal = "10" };
      { expected_type = SEMICOLON; expected_literal = ";" };
      { expected_type = VAL; expected_literal = "val" };
      { expected_type = IDENT; expected_literal = "add" };
      { expected_type = ASSIGN; expected_literal = "=" };
      { expected_type = FUNCTION; expected_literal = "fn" };
      { expected_type = LPAREN; expected_literal = "(" };
      { expected_type = IDENT; expected_literal = "x" };
      { expected_type = COMMA; expected_literal = "," };
      { expected_type = IDENT; expected_literal = "y" };
      { expected_type = RPAREN; expected_literal = ")" };
      { expected_type = LBRACE; expected_literal = "{" };
      { expected_type = IDENT; expected_literal = "x" };
      { expected_type = PLUS; expected_literal = "+" };
      { expected_type = IDENT; expected_literal = "y" };
      { expected_type = SEMICOLON; expected_literal = ";" };
      { expected_type = RBRACE; expected_literal = "}" };
      { expected_type = SEMICOLON; expected_literal = ";" };
      { expected_type = VAL; expected_literal = "val" };
      { expected_type = IDENT; expected_literal = "res" };
      { expected_type = ASSIGN; expected_literal = "=" };
      { expected_type = IDENT; expected_literal = "add" };
      { expected_type = LPAREN; expected_literal = "(" };
      { expected_type = IDENT; expected_literal = "five" };
      { expected_type = COMMA; expected_literal = "," };
      { expected_type = IDENT; expected_literal = "ten" };
      { expected_type = RPAREN; expected_literal = ")" };
      { expected_type = SEMICOLON; expected_literal = ";" };
      { expected_type = EOF; expected_literal = "" };
    ]
  in

  let l = Fluff.Lexer.new_lexer input in

  List.iteri
    (fun i tt ->
      let _, tok = Fluff.Lexer.next_token l in

      Alcotest.check string
        ("token literal " ^ string_of_int i)
        tt.expected_literal
        (Fluff.Token.get_token_literal tok);

      Alcotest.check token_type_testable
        ("token type " ^ string_of_int i)
        tt.expected_type
        (Fluff.Token.get_token_type tok))
    tests

let () =
  Alcotest.run "Lexer tests"
    [
      ( "test_lexer_simple",
        [ Alcotest.test_case "simple input" `Quick test_lexer_simple ] );
      ("next_token", [ Alcotest.test_case "basic input" `Quick test_next_token ]);
    ]
