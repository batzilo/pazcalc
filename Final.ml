open Identifier
open Printf
open SemQuad
open Symbol

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
let name p =
    let num = !nat in
    incnat ();
    "_" ^ id_name p ^ "_" ^ string_of_int num

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
            name e.entry_id ^ "\
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
            endof e.entry_id ^ ": \
            \tmov sp, bp\n\
            \tpop bp\n\
            \tret\n" ^
            name e.entry_id ^
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
        ""
    | Q_par (x,m) ->
        ""
    | Q_ret ->
        ""

let generate name icode =
    header name ^ List.fold_left (^) "" (List.map transform icode) ^ footer
