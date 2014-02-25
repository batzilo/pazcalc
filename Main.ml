(*
 * file : Main.ml
 *)

open Final
open Lexer
open Parser
open Printf
open SemQuad

let main () =
    match Array.length Sys.argv with
    | 2 ->
        begin
        let file = Sys.argv.(1) in
        let name =
            try
                let pos = String.rindex file '.' in
                String.sub file 0 pos
            with Not_found ->
                file
        in
        let iname = name ^ ".imm" in
        let fname = name ^ ".asm" in
        (* printf "file = \"%s\"\nname is = \"%s\"\niname = \"%s\"\nfname = \"%s\"\n" file name iname fname; *)
        let cin = open_in file in
        let couti = open_out iname in
        let coutf = open_out fname in
        let lexbuf = Lexing.from_channel cin in
        try
            let intermediate_code = Parser.pazprog Lexer.pazcal lexbuf in
            let final_code = Final.generate intermediate_code in
            fprintf couti "%s" (printIntermediateCode ());
            fprintf coutf "%s" final_code;
            ()
        with Parsing.Parse_error ->
            printf "\n\tA syntax error occured!\n\n"
        end
    | _ ->
        printf "Usage : $ ./pazcalc source_code.paz\n"

let _ = main ()
