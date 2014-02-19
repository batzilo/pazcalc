open Error
open Identifier
open Printf
open SemQuad
open Symbol
open Types

(* beggining *)
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

(* end *)
let footer = "xseg\tends\n\tend\tmain";;



(* assembly label numbers *)
let nat = ref 0;;

let incnat () =
    !nat <- !nat + 1

(* current unit label *)
let curr = ref "";;



(* helper functions *)

(* used for non local operands *)
let getAR () = "\tmov si, word ptr [bp + 4]\n"

(* update Access Links *)
let updateAL () = "\tpush word ptr [bp+4]\n"

let operand_size e =
    match e.entry_info with
    | ENTRY_variable inf -> if sizeOfType inf.variable_type = 1 then "byte" else "word"
    | ENTRY_parameter inf -> if sizeOfType inf.parameter_type = 1 then "byte" else "word"
    | ENTRY_temporary inf -> if sizeOfType inf.temporary_type = 1 then "byte" else "word"
    | _ -> "error"

let rec load r a =
    match a with
    | Q_int i ->    "\tmov " ^ r ^ ", " ^ string_of_int i ^ "\n"
    | Q_bool b ->   "\tmov " ^ r ^ ", " ^ ( if b then "1" else "0" ) ^ "\n"
    | Q_char c ->   "\tmov " ^ r ^ ", " ^ string_of_int (int_of_char c) ^ "\n"
    | Q_deref e ->  load "di" (Q_entry e) ^ "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [di]\n"
    | Q_entry e ->
        begin
            if e.entry_scope.sco_nesting = 1 then
                (* local operand *)
                match e.entry_info with
                | ENTRY_variable inf ->
                    "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [bp + " ^ string_of_int inf.variable_offset ^ "]\n"
                | ENTRY_parameter inf ->
                    if inf.parameter_mode = PASS_BY_VALUE then
                        "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [bp + " ^ string_of_int inf.parameter_offset ^ "]\n"
                    else
                        "\tmov si, word ptr [bp + " ^ string_of_int inf.parameter_offset ^ "]\n" ^
                        "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [si]\n"
                | ENTRY_temporary inf ->
                    "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [bp + " ^ string_of_int inf.temporary_offset ^ "]\n"
                | _ ->
                    "<error>"
            else
                (* non-local operand *)
                match e.entry_info with
                | ENTRY_variable inf ->
                    getAR () ^ 
                    "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [si + " ^ string_of_int inf.variable_offset ^ "]\n"
                | ENTRY_parameter inf ->
                    if inf.parameter_mode = PASS_BY_VALUE then
                        getAR () ^
                        "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [si + " ^ string_of_int inf.parameter_offset ^ "]\n"
                    else
                        getAR () ^
                        "\tmov si, word ptr [si + " ^ string_of_int inf.parameter_offset ^ "]\n" ^
                        "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [si]\n"
                | ENTRY_temporary inf ->
                    getAR () ^
                    "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [bp + " ^ string_of_int inf.temporary_offset ^ "]\n"
                | _ ->
                    "<error>"
        end
    | _ -> "<oops>\n"

let loadAddr r a =
    match a with
    | Q_string s -> "\tlea " ^ r ^ "byte ptr " ^ s
    | Q_deref x -> load r (Q_entry x)
    | Q_entry e ->
        begin
            if e.entry_scope.sco_nesting = 1 then
                (* local operand *)
                match e.entry_info with
                (* FIXME why not?
                | ENTRY_variable inf ->
                    "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [bp + " ^ string_of_int inf.variable_offset ^ "]\n"
                *)
                | ENTRY_parameter inf ->
                    if inf.parameter_mode = PASS_BY_VALUE then
                        "\tlea " ^ r ^ ", " ^ operand_size e ^ " ptr [bp + " ^ string_of_int inf.parameter_offset ^ "]\n"
                    else
                        "\tmov " ^ r ^ ", word ptr [bp + " ^ string_of_int inf.parameter_offset ^ "]\n"
                | ENTRY_temporary inf ->
                    "\tlea " ^ r ^ ", " ^ operand_size e ^ " ptr [bp + " ^ string_of_int inf.temporary_offset ^ "]\n"
                | _ ->
                    "<error>"
            else
                (* non-local operand *)
                match e.entry_info with
                (*
                | ENTRY_variable inf ->
                    getAR () ^ 
                    "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [si + " ^ string_of_int inf.variable_offset ^ "]\n"
                *)
                | ENTRY_parameter inf ->
                    if inf.parameter_mode = PASS_BY_VALUE then
                        getAR () ^
                        "\tlea " ^ r ^ ", " ^ operand_size e ^ " ptr [si + " ^ string_of_int inf.parameter_offset ^ "]\n"
                    else
                        getAR () ^
                        "\tmov " ^ r ^ ", word ptr [si + " ^ string_of_int inf.parameter_offset ^ "]\n"
                | ENTRY_temporary inf ->
                    getAR () ^
                    "\tlea " ^ r ^ ", " ^ operand_size e ^ " ptr [bp + " ^ string_of_int inf.temporary_offset ^ "]\n"
                | _ ->
                    "<error>"
        end
    | _ -> "<oops>\n"

let store r a =
    match a with
    | Q_funct_res -> "\tmov word ptr [bp + 6], " ^ r ^ "\n"
    | Q_deref e ->  load "di" (Q_entry e) ^ "\tmov " ^ operand_size e ^ " ptr [di], " ^ r ^ "\n" 
    | Q_entry e ->
        begin
            if e.entry_scope.sco_nesting = 1 then
                (* local operand *)
                match e.entry_info with
                (*
                | ENTRY_variable inf ->
                    "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [bp + " ^ string_of_int inf.variable_offset ^ "]\n"
                *)
                | ENTRY_parameter inf ->
                    if inf.parameter_mode = PASS_BY_VALUE then
                        "\tmov " ^ operand_size e ^ " ptr [bp + " ^ string_of_int inf.parameter_offset ^ "], " ^ r ^ "\n"
                    else
                        "\tmov si, word ptr [bp + " ^ string_of_int inf.parameter_offset ^ "]\n" ^
                        "\tmov " ^ operand_size e ^ " ptr [si], " ^ r ^ "\n"
                | ENTRY_temporary inf ->
                    "\tmov " ^ operand_size e ^ " ptr [bp + " ^ string_of_int inf.temporary_offset ^ "], " ^ r ^ "\n"
                | _ ->
                    "<error>"
            else
                (* non-local operand *)
                match e.entry_info with
                | ENTRY_variable inf ->
                    getAR () ^ 
                    "\tmov " ^ operand_size e ^ " ptr [si + " ^ string_of_int inf.variable_offset ^ "], " ^ r ^ "\n"
                | ENTRY_parameter inf ->
                    if inf.parameter_mode = PASS_BY_VALUE then
                        getAR () ^
                        "\tmov " ^ operand_size e ^ " ptr [si + " ^ string_of_int inf.parameter_offset ^ "], " ^ r ^ "\n"
                    else
                        getAR () ^
                        "\tmov si, word ptr [si + " ^ string_of_int inf.parameter_offset ^ "]\n" ^
                        "\tmov " ^ operand_size e ^ " ptr [si], " ^ r ^ "\n"
                | ENTRY_temporary inf ->
                    getAR () ^ 
                    "\tmov " ^ operand_size e ^ " ptr [si + " ^ string_of_int inf.temporary_offset ^ "], " ^ r ^ "\n"
                | _ ->
                    "<error>"
        end
    | _ -> "<oops>\n"

(* produce a unique assembly label for the beginning of procedure p *)
let name e =
    match e.entry_info with
    | ENTRY_function inf ->
        begin
        begin
        match inf.function_scope with
        | Some sco -> currentScope := sco
        | None -> internal "dafuq?"
        end;
        match inf.function_label with
        | Some c ->
            "_" ^ id_name e.entry_id ^ "_" ^ string_of_int c
        | None ->
            incnat ();
            inf.function_label <- Some !nat;
            "_" ^ id_name e.entry_id ^ "_" ^ string_of_int !nat
        end
    | _ -> "entry info not a function!"

(* produce a unique assembly label for the end of procedure p *)
let endof e =
    (* "@" ^ id_name e ^ "_" ^ string_of_int !nat *)
    match e.entry_info with
    | ENTRY_function inf ->
        begin
        match inf.function_label with
        | Some c ->
            "@" ^ id_name e.entry_id ^ "_" ^ string_of_int c
        | None ->
            "<end of un-started unit?>"
        end
    | _ -> "entry info not a function!"

(* size of local and temporary variables of unit x *)
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

(* increase stack pointer by two if callee is a procedure *)
let sub_if_proc e =
    match e.entry_info with
    | ENTRY_function inf ->
        begin
        match inf.function_result with
        | TYPE_proc -> "\tsub sp, 2\n"
        | _ -> ""
        end
    | _ -> "entry is not a function!"

(* map quads to assembly *)
let transform (i,quad) =
    "@" ^ string_of_int i ^ ":\n" ^
    match quad with
    | Q_empty ->
        ""
    | Q_unit u ->
        begin
        match u with
        | Q_entry e ->
            let n = name e in
            !curr <- endof e;
            n ^ "\
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
            endof e ^ ":\
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
        begin
        (*
        match x,z with
        | Q_entry x1, Q_entry z1 ->
            let sz =
                match z1.entry_info with
                | ENTRY_variable inf -> sizeOfType inf.variable_type
                | ENTRY_parameter inf -> sizeOfType inf.parameter_type
                | ENTRY_temporary inf -> sizeOfType inf.temporary_type
                | _ -> 2
            in 
            if (sz = 1)
                then (load "al" x) ^ (store "al" z)
                else (load "ax" x) ^ (store "ax" z)
        | _ -> "<error>"
        *)
        match z with
        | Q_entry z1 ->
            let sz =
                match z1.entry_info with
                | ENTRY_variable inf -> sizeOfType inf.variable_type
                | ENTRY_parameter inf -> sizeOfType inf.parameter_type
                | ENTRY_temporary inf -> sizeOfType inf.temporary_type
                | _ -> 2
            in 
            if (sz = 1)
                then (load "al" x) ^ (store "al" z)
                else (load "ax" x) ^ (store "ax" z)
        | Q_funct_res ->
            load "ax" x ^ store "ax" z
        | _ -> "<error>"
        end
    | Q_array (x,y,z) ->
        ""
    | Q_relop (op,x,y,z) ->
        ""
    | Q_ifb (x,z) ->
        ""
    | Q_jump z ->
        begin
        match z with
        | Q_int i ->
            "\tjmp @" ^ string_of_int i ^ "\n"
        | _ ->
            "<jmp error>"
        end
    | Q_label l ->
        "< label unsupported by pazcal>"
    | Q_jumpl l ->
        "< jumpl unsupported by pazcal>"
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
        "\tjmp " ^ !curr ^ "\n"

let generate name icode =
    header name ^ List.fold_left (^) "" (List.map transform icode) ^ footer
