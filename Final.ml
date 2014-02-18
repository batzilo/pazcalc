open Identifier
open Printf
open SemQuad
open Symbol
open Types

let header prog_name = 
    sprintf "xseg\tsegment\tpublic 'code'\n\
    \tassume\tcs:xseg, ds:xseg, ss:xseg\n\
    \torg\t100h\n\
    main\tproc\tnear\n\
    \tcall\tnear ptr %s\n\
    \tmov\tax,4C00h\n\
    \tint\t21h\n\
    main\tendp\n"
    prog_name

let footer = "xseg\tends\n\tend\tmain";;

let nat = ref 0;;

let incnat () =
    !nat <- !nat + 1

(* produces a unique assembly label for procedure p *)
let name e =
    match e.entry_info with
    | ENTRY_function inf ->
        begin
        match inf.function_label with
        | Some c ->
            "_" ^ id_name e.entry_id ^ "_" ^ string_of_int c
        | None ->
            incnat ();
            inf.function_label <- Some !nat;
            "_" ^ id_name e.entry_id ^ "_" ^ string_of_int !nat
        end
    | _ -> "entry info not a function!"

let size x =
    match x.entry_info with
    | ENTRY_function inf ->
        begin
        match inf.function_scope with
        | Some sco ->
            let sz = sco.sco_negofs in
            string_of_int (-sz)
        | _ -> "-1"
        end
    | _ -> "-1"

let sub_if_proc e =
    match e.entry_info with
    | ENTRY_function inf ->
        begin
        match inf.function_result with
        | TYPE_proc -> "\tsub sp, 2\n"
        | _ -> ""
        end
    | _ -> "entry is not a function!"

let updateAL () = "\tpush word ptr[bp+4]\n"

let endof p =
    "@" ^ id_name p ^ "_" ^ string_of_int !nat

let transform (i,quad) =
    "@" ^ string_of_int i ^ ":\n" ^
    match quad with
    | Q_empty ->
        ""
    | Q_unit u ->
        begin
        match u with
        | Q_entry e ->
            name e ^ "\
            \tproc near\n\
            \tpush bp\n\
            \tmov bp, sp\n\
            \tsub sp, " ^ size e ^ "\n"
        | _ ->
            "<error>"
        end
    | Q_endu u ->
        begin
        match u with
        | Q_entry e ->
            endof e.entry_id ^ ":\
            \tmov sp, bp\n\
            \tpop bp\n\
            \tret\n" ^
            name e ^
            "\tendp\n"
        | _ ->
            "<error>"
        end
    | Q_op (op,x,y,z) ->
        ""
    | Q_assign (x,z) ->
        ""
    | Q_array (x,y,z) ->
        ""
    | Q_relop (op,x,y,z) ->
        ""
    | Q_ifb (x,z) ->
        ""
    | Q_jump z ->
        ""
    | Q_label l ->
        ""
    | Q_jumpl l ->
        ""
    | Q_call u ->
        begin
        match u with
        | Q_entry e ->
            sub_if_proc e ^
            updateAL () ^
            "\tcall near ptr " ^ name e ^
            "\n\tadd sp, " ^ string_of_int (int_of_string (size e) + 4) ^ "\n"
        | _ ->
            "<error>"
        end
    | Q_par (x,m) ->
        ""
    | Q_ret ->
        ""

let generate name icode =
    header name ^ List.fold_left (^) "" (List.map transform icode) ^ footer
