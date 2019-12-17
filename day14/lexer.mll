{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

rule read =
  parse
  | ' ' { read lexbuf }
  | '\n' { next_line lexbuf; read lexbuf }
  | "=>" { ARROW }
  | ',' { COMMA }
  | ['0'-'9']+ as i { INT (int_of_string i) }
  | ['A'-'Z']+ as s { IDENT (s) }
  | _ { raise (SyntaxError ("Unexpected : " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
