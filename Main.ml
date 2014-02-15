(*
 * file : Main.ml
 *)

open Lexer
open Parser
open Printf

(*
let main () =
  try
    let cin = if Array.length Sys.argv > 1 then open_in Sys.argv.(1) else stdin in
      let lexbuf = Lexing.from_channel cin in
        while true do
          try
            Parser.pazprog Lexer.pazcal lexbuf
          with Parsing.Parse_error -> 
            printf "\n\tSyntax error!\n\n";
            exit 0
        done
  with End_of_file ->
      printf "\nFinished.\n\n";
      exit 0

let _ = Printexc.print main ()
*)

let main () =
  let cin = if Array.length Sys.argv > 1 then open_in Sys.argv.(1) else stdin in
    let lexbuf = Lexing.from_channel cin in
      try
        let intermediate_code = Parser.pazprog Lexer.pazcal lexbuf in
        let final_code = Final.generate "foo" intermediate_code in
        printf "%s\n" final_code
      with Parsing.Parse_error ->
        printf "\n\tA Syntax Error Occured!\n\n";
        exit 0

let _ = main ()
