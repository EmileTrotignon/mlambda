let _print_token (tok : Parser.token) =
  ( match tok with
  | LOWERCASE_ID id ->
      Printf.printf "LOWERCASE_ID (%s)" id
  | UPPERCASE_ID cons ->
      Printf.printf "UPPERCASE_ID (%s)" cons
  | INT i ->
      Printf.printf "INT(%i)" i
  | STRING str ->
      Printf.printf "STRING(%S)" str
  | BOOL b ->
      Printf.printf "BOOL(%s)" (if b then "true" else "false")
  | LET ->
      print_string "LET"
  | REC ->
      print_string "REC"
  | AND ->
      print_string "AND"
  | IN ->
      print_string "IN"
  | FUN ->
      print_string "FUN"
  | UNDERSCORE ->
      print_string "UNDERSCORE"
  | IF ->
      print_string "IF"
  | THEN ->
      print_string "THEN"
  | ELSE ->
      print_string "ELSE"
  | MATCH ->
      print_string "MATCH"
  | WITH ->
      print_string "WITH"
  | LARROW ->
      print_string "LARROW"
  | RARROW ->
      print_string " RARROW"
  | ALLOC ->
      print_string "ALLOC"
  | DOT ->
      print_string "DOT"
  | PIPE ->
      print_string "PIPE"
  | EQUAL ->
      print_string "EQUAL"
  | COMMA ->
      print_string "COMMA"
  | SEMI ->
      print_string "SEMI"
  | LPARRPAR ->
      print_string "LPARRPAR"
  | LPAR ->
      print_string "LPAR"
  | RPAR ->
      print_string "RPAR"
  | EOF ->
      print_string "EOF" ) ;
  print_newline ()

let token buf =
  let tok = Lexer.token buf in
  (*_print_token tok ;*) tok

let lexbuf buf = Parser.program token buf

let string str =
  let buf = Lexing.from_string str in
  lexbuf buf

let channel ch = Lexing.from_channel ch |> lexbuf

let file name =
  let file = open_in name in
  let r = channel file in
  close_in file ; r
