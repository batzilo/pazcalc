(*
 * Pazcal Lectical Analyzer - lexer.mll
 *
 * compile with : $ ocamllex lexer.mll
 * run with : $ ocaml lexer.ml
 *
 *)

(* header section *)
{
  open Printf
  open Lexing

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
      List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
      tbl

  type token =
    | T_and | T_bool | T_break | T_case | T_char | T_const
    | T_continue | T_default | T_do | T_DOWNTO | T_else | T_false
    | T_FOR | T_FORM | T_FUNC | T_if | T_int | T_MOD 
    | T_NEXT | T_not | T_or | T_PROC | T_PROGRAM | T_REAL
    | T_return | T_STEP | T_switch | T_TO | T_true | T_while
    | T_WRITE | T_WRITELN | T_WRITESP | T_WRITESPLN
   
    | T_id of string | T_int_const of int | T_float_const of float
    | T_char_const of char | T_string_const of string
   
    | T_eq | T_gr | T_ls | T_neq | T_greq | T_lseq | T_plus | T_minus 
    | T_mul | T_div | T_mod | T_lg_not | T_lg_and | T_lg_or | T_plus_plus 
    | T_minus_minus | T_assign | T_plus_assign | T_minus_assign | T_mul_assign 
    | T_div_assign | T_mod_assign

    | T_amp | T_sem_col | T_dot | T_lparen | T_rparen | T_col
    | T_comma | T_lbrack | T_rbrack | T_lbrace | T_rbrace

  let keyword_table =
    create_hashtable 64 [
      ("and", T_and); ("bool", T_bool); ("else", T_break); ("case", T_case);
      ("char", T_char); ("const", T_const); ("continue", T_continue); ("default", T_default);
      ("do", T_do); ("DOWNTO", T_DOWNTO); ("else", T_else); ("false", T_false);
      ("FOR", T_FOR); ("FORM", T_FORM); ("FUNC", T_FUNC); ("if", T_if);
      ("int", T_int); ("MOD", T_MOD); ("NEXT", T_NEXT); ("not", T_not);
      ("or", T_or); ("PROC", T_PROC); ("PROGRAM", T_PROGRAM); ("REAL", T_REAL);
      ("return", T_return); ("STEP", T_STEP); ("switch", T_switch); ("TO", T_TO); 
      ("true", T_true); ("while", T_while); ("WRITE", T_WRITE); ("WRITELN", T_WRITELN); 
      ("WRITESP", T_WRITESP); ("WRITESPLN", T_WRITESPLN);
   ]
}

(* definitions section *)
  let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* 
  let digit = ['0'-'9']
  let common = [^ ''' '"' '\\']
  let escape = '\\'['n' 't' 'r' '0' '\\' ''' '"']
  let white = [' ' '\t' '\r']

(* rules section *)
  rule pazcal = parse
  (*identifiers / keywords *)
    | id as word
     { try 
       let token = Hashtbl.find keyword_table word in
       printf "keyword: %s\n" word;
       token
       with Not_found ->
       printf "identifier: %s\n" word;
       T_id word
     }
  (* non-zero integer constants can not begin with '0' *)
    | '0'digit+
     {
      printf "Lexical error in line %d: Non-zero integer constants cannot begin with a '0'\n" lexbuf.lex_curr_p.pos_lnum;
      pazcal lexbuf
     } 
  (* integer constants *)
    | digit+ as inum
     { let num = int_of_string inum in
       printf "integer constant: %d\n" num;
       T_int_const num
     }
  (* float constants *)
    | (digit+)'.'(digit+)(['E''e']['+''-']? digit+)? as fnum
     { let num = float_of_string fnum in
       printf "float constant: %f\n" num;
       T_float_const num
     }
  (* new line *)
    | '\n'
     { new_line lexbuf;
       pazcal lexbuf
     }
  (*char constants except '\n' *)
    | '''(common|escape)''' as cc
     { printf "char constant: %s\n" cc;
       T_char_const (lexeme_char lexbuf 1)
     }
  (* string constants *)
    | '"'((common # '\n') | escape)*'"' as sc
     { printf "string constant: %s\n" sc;
       T_string_const sc
     }
  (* operators  *)
    | "==" as op { printf "operator: %s\n" op; T_eq }
    | '>'  as op { printf "operator: %c\n" op; T_gr } 
    | '<'  as op { printf "operator: %c\n" op; T_ls }
    | "!=" as op { printf "operator: %s\n" op; T_neq }
    | ">=" as op { printf "operator: %s\n" op; T_greq }
    | "<=" as op { printf "operator: %s\n" op; T_lseq }
    | '+'  as op { printf "operator: %c\n" op; T_plus }
    | '-'  as op { printf "operator: %c\n" op; T_minus }
    | '*'  as op { printf "operator: %c\n" op; T_mul }
    | '/'  as op { printf "operator: %c\n" op; T_div }
    | '%'  as op { printf "operator: %c\n" op; T_mod }
    | '!'  as op { printf "operator: %c\n" op; T_lg_not }
    | "&&" as op { printf "operator: %s\n" op; T_lg_and }
    | "||" as op { printf "operator: %s\n" op; T_lg_or }
    | "++" as op { printf "operator: %s\n" op; T_plus_plus }
    | "--" as op { printf "operator: %s\n" op; T_minus_minus }
    | '='  as op { printf "operator: %c\n" op; T_assign }
    | "+=" as op { printf "operator: %s\n" op; T_plus_assign }
    | "-=" as op { printf "operator: %s\n" op; T_minus_assign }
    | "*=" as op { printf "operator: %s\n" op; T_mul_assign }
    | "/=" as op { printf "operator: %s\n" op; T_div_assign }
    | "%=" as op { printf "operator: %s\n" op; T_mod_assign }
  (* separators *)
    | '&' as sep { printf "separator: %c\n" sep; T_amp }
    | ';' as sep { printf "separator: %c\n" sep; T_sem_col }
    | '.' as sep { printf "separator: %c\n" sep; T_dot }
    | '(' as sep { printf "separator: %c\n" sep; T_lparen }
    | ')' as sep { printf "separator: %c\n" sep; T_rparen }
    | ':' as sep { printf "separator: %c\n" sep; T_col }
    | ',' as sep { printf "separator: %c\n" sep; T_comma }
    | '[' as sep { printf "separator: %c\n" sep; T_lbrack }
    | ']' as sep { printf "separator: %c\n" sep; T_rbrack }
    | '{' as sep { printf "separator: %c\n" sep; T_lbrace }
    | '}' as sep { printf "separator: %c\n" sep; T_rbrace }
  (* eat up white space characters *)
    | white+
     { pazcal lexbuf }
  (* eat up one-line comments *)
    | "//"[^'\n']*
     { pazcal lexbuf }
  (* activate "comment" rule *)
    | "/*" 
     { comment lexbuf } 
  (* eof *)
    | eof { raise End_of_file }
   (* comment WTF *)
    | "*/" { printf "Lexical error in line %d: Shouldn't have reached here. Check 'comment' entrypoint.\n" lexbuf.lex_curr_p.pos_lnum; pazcal lexbuf }
   (* anything else *)
    | _ as chr
     { printf "Lexical error in line %d: Unrecognized character: %c \n" lexbuf.lex_curr_p.pos_lnum chr;
       pazcal lexbuf
     }
  and comment = parse
  (* go back to "pazcal" rule *)
    | "*/" { pazcal lexbuf }
  (* or keep on "eating" comments *)
    | '\n' 
     { new_line lexbuf; 
       comment lexbuf 
     }
    | _ { comment lexbuf }

(* trailer section *)
{
  let rec parse lexbuf = 
    let token = pazcal lexbuf in
    (* do nothing in this example *)
    parse lexbuf

  let main () = 
    let cin = 
       if Array.length Sys.argv > 1
       then open_in Sys.argv.(1)
       else stdin
    in
    let lexbuf = Lexing.from_channel cin in
      try parse lexbuf
      with End_of_file -> ()
    
  let _ = Printexc.print main ()
}
