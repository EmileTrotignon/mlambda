{ (* -*- tuareg -*- *)
  open Parser
  let char_of_atom _ atom =
  match atom with
  | {|\n|} -> '\n'
  | {|\t|} -> '\t'
  | {|\b|} -> '\b'
  | {|\r|} -> '\r'
  | {|\\|} -> '\\'
  | {|\'|} -> '\''
  | {|\"|} -> '"'
  | _ when String.length atom = 1 -> atom.[0]
  | _ when atom.[0] = {|\|}.[0] -> (
      try Char.chr (int_of_string (String.sub atom 1 (String.length atom - 1)))
      with Invalid_argument _ ->
        failwith "Lexing error : invalid" )
  | _ -> failwith "Should never happen"
}

let digit = ['0' - '9']
let hexdigit = digit | ['a' - 'f'] | ['A' - 'F']
let lowercase_letter = ['a' - 'z']
let uppercase_letter = ['A' - 'Z']
let letter = lowercase_letter | uppercase_letter
let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012'] | '\n'
let atom_code = ('\\' digit (digit ?)  (digit ?)) | ("\\0x" hexdigit (hexdigit ?))
let underscore = "_"
let printable = [' ' - '~']
let char_printable = [' ' - '&' '(' - '~']
let string_printable = [' ' - '!' '#' - '~']
let atom = char_printable | atom_code | "\\t" | "\\r" | "\\b" | "\\n" | "\\'" | "\\\\"
let string_atom = string_printable | atom_code | "\\t" | "\\r" | "\\b" | "\\n" | "\\\"" | "\\\\"
let char = "'" atom "'"
let lpar = "("
let rpar = ")"
let backslash = "\\"
let ampersand = "&"
let double_ampersand = "&&"
let pipepipe = "||"
let equal_question = "=?"
let lbrack_equal_question = "<=?"
let rbrack_equal_question = ">=?"
let lbrack_question = "<?"
let rbrack_question = ">?"
let open_com = "/*"
let close_com = "*/"
let dot = "."
let exclamation = "!"
let pipe = "|"
let colon = ":"
let semi = ";"
let equal = "="
let plus = "+"
let minus = "-"
let star = "*"
let slash = "/"
let langle = "<"
let rangle = ">"
let rarrow = "->"
let larrow = "<-"
let comma = ","
let lowercase_id = lowercase_letter ((letter | digit | '_' | '\'' )* )
let uppercase_id = uppercase_letter ((letter | digit | '_' | '\'' )* )

let number = '-'? ((digit+) | ("0x" (hexdigit+)) | ("0b" (('0' | '1')+)) | ("0o" (['0' - '7']+)))

rule comment depth = parse
  | open_com  { comment (depth + 1) lexbuf        }
  | close_com { if depth = 0 then
                  ( token lexbuf )
                else
                  ( comment (depth  - 1) lexbuf ) }
  | _         { comment depth lexbuf              }

and string accumulator = parse
 | "\""        { STRING(
                  String.of_seq
                    (List.to_seq
                      (List.map (char_of_atom lexbuf)
                        (List.rev accumulator)))) }
 | string_atom { string ((Lexing.lexeme lexbuf) :: accumulator) lexbuf }
 | eof         { failwith "Lexing error : string unfinished" }

and token = parse
  (** Layout *)
  | blank+                { token lexbuf     }
  | open_com              { comment 0 lexbuf }
  (* char *)
(*| "'" atom "'"          { CHAR (char_of_atom lexbuf (unqote(Lexing.lexeme lexbuf))) }*)
  | "\""                  { string [] lexbuf }
  | number                { try
                              INT(int_of_string (Lexing.lexeme lexbuf))
                            with Failure _ ->
                              failwith "Lexing : int_of_string failure" }
  (* atomic lexemes *)
  | "let"    { LET          }
  | "rec"    { REC          }
  | "fun"    { FUN          }
  | "in"     { IN           }
  | "if"     { IF           }
  | "then"   { THEN         }
  | "else"   { ELSE         }
  | "match"  { MATCH        }
  | "with"   { WITH         }
  | "alloc"  { ALLOC        }
  | "()"     { LPARRPAR     }
  | "true"   { BOOL (true)  }
  | "false"  { BOOL (false) }
  | comma    { COMMA        }
  | semi     { SEMI         }
  | rarrow   { RARROW       }
  | larrow   { LARROW       }
  | lpar     { LPAR         }
  | rpar     { RPAR         }
  | dot      { DOT          }
  | equal    { EQUAL        }
  | pipe     { PIPE         }
  | eof      { EOF          }
  (* identifiers *)
  | lowercase_id     { LOWERCASE_ID(Lexing.lexeme lexbuf) }
  | uppercase_id     { UPPERCASE_ID(Lexing.lexeme lexbuf) }
  | "::"             { UPPERCASE_ID("::")                 }
  | "[]"             { UPPERCASE_ID("[]")                 }
  | underscore       { UNDERSCORE                         }
  (** Lexing error. *)
  | _ { failwith (Printf.sprintf "Lexing error: %S" (Lexing.lexeme lexbuf)) }

