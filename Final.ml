open Error
open Identifier
open Printf
open SemQuad
open Symbol
open Types

let reg_lib = "\n\n \
    extrn __new : proc\n \
    extrn __dispose : proc\n \
    extrn _formatInteger : proc\n \
    extrn _formatReal : proc\n \
    extrn _parseInteger : proc\n \
    extrn _parseReal : proc\n \
    \n \
    extrn _abs : proc\n \
    extrn _arctan : proc\n \
    extrn _cos : proc\n \
    extrn _exp : proc\n \
    extrn _fabs : proc\n \
    extrn _ln : proc\n \
    extrn _pi : proc\n \
    extrn _sin : proc\n \
    extrn _sqrt : proc\n \
    extrn _tan : proc\n \
    \n \
    extrn _writeBoolean : proc\n \
    extrn _writeChar : proc\n \
    extrn _writeInteger : proc\n \
    extrn _writeReal : proc\n \
    extrn _writeString : proc\n \
    extrn _readBoolean : proc\n \
    extrn _readChar : proc\n \
    extrn _readInteger : proc\n \
    extrn _readReal : proc\n \
    extrn _readString : proc\n \
    \n \
    extrn _chr : proc\n \
    extrn _exit : proc\n \
    extrn _ord : proc\n \
    extrn _round : proc\n \
    extrn _trunc : proc\n \
    ";;

let main = ref "";;

let mapPredefToLib s =
    match s with
    | "putchar" -> "writeChar"
    | "puts" -> "writeString"
    | "READ_INT" -> "readInteger"
    | "READ_BOOL" -> "readBoolean"
    | "getchar" -> "readCharacter"
    | "READ_REAL" -> "readReal"
    | _ -> s

(* beggining *)
let header () = 
    sprintf "\
    ; x86 assembly - intel syntax\n\
    xseg\tsegment\tpublic 'code'\n\
    \tassume\tcs:xseg, ds:xseg, ss:xseg\n\
    \torg\t100h\n\
    main\tproc\tnear\n\
    \tcall\tnear ptr %s\n\
    \tmov\tax,4C00h\n\
    \tint\t21h\n\
    main\tendp\n"
    !main

(* end *)
let footer () = reg_lib ^ "\nxseg\tends\n\tend\tmain\n";;



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
let updateAL () = "\tmov si, word ptr [bp + 4]\n\tpush word ptr [si + 4]\n"

let operand_size e =
    match e.entry_info with
    | ENTRY_variable inf -> if sizeOfType inf.variable_type = 1 then "byte" else "word"
    | ENTRY_parameter inf -> if sizeOfType inf.parameter_type = 1 then "byte" else "word"
    | ENTRY_temporary inf -> if sizeOfType inf.temporary_type = 1 then "byte" else "word"
    | _ -> "<error>"

let fix_offset a =
    if a > 0
        then "+ " ^ string_of_int a
        else "- " ^ string_of_int (-a)

let rec load r a =
    match a with
    | Q_int i ->    "\tmov " ^ r ^ ", " ^ string_of_int i ^ "\n"
    | Q_bool b ->   "\tmov " ^ r ^ ", " ^ ( if b then "1" else "0" ) ^ "\n"
    | Q_char c ->   "\tmov " ^ r ^ ", " ^ string_of_int (int_of_char c) ^ "\n"
    | Q_deref e ->  load "di" (Q_entry e) ^ "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [di]\n"
    | Q_entry e ->
        begin
            if e.entry_scope.sco_nesting = 2 then
                (* local operand *)
                match e.entry_info with
                | ENTRY_variable inf ->
                    "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [bp " ^ fix_offset inf.variable_offset ^ "]\n"
                | ENTRY_parameter inf ->
                    if inf.parameter_mode = PASS_BY_VALUE then
                        "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [bp " ^ fix_offset  inf.parameter_offset ^ "]\n"
                    else
                        "\tmov si, word ptr [bp " ^ fix_offset inf.parameter_offset ^ "]\n" ^
                        "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [si]\n"
                | ENTRY_temporary inf ->
                    "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [bp " ^ fix_offset inf.temporary_offset ^ "]\n"
                | _ ->
                    "<error>\n"
            else
                (* non-local operand *)
                match e.entry_info with
                | ENTRY_variable inf ->
                    getAR () ^ 
                    "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [si " ^ fix_offset inf.variable_offset ^ "]\n"
                | ENTRY_parameter inf ->
                    if inf.parameter_mode = PASS_BY_VALUE then
                        getAR () ^
                        "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [si " ^ fix_offset inf.parameter_offset ^ "]\n"
                    else
                        getAR () ^
                        "\tmov si, word ptr [si " ^ fix_offset inf.parameter_offset ^ "]\n" ^
                        "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [si]\n"
                | ENTRY_temporary inf ->
                    getAR () ^
                    "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [bp " ^ fix_offset inf.temporary_offset ^ "]\n"
                | _ ->
                    "<error>\n"
        end
    | _ -> "<oops>\n"

let loadAddr r a =
    match a with
    | Q_string s -> "\tlea " ^ r ^ " byte ptr " ^ s ^ "\n"
    | Q_deref x -> load r (Q_entry x)
    | Q_entry e ->
        begin
            if e.entry_scope.sco_nesting = 2 then
                (* local operand *)
                match e.entry_info with
                | ENTRY_variable inf ->
                    "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [bp " ^ fix_offset inf.variable_offset ^ "]\n"
                | ENTRY_parameter inf ->
                    if inf.parameter_mode = PASS_BY_VALUE then
                        "\tlea " ^ r ^ ", " ^ operand_size e ^ " ptr [bp " ^ fix_offset inf.parameter_offset ^ "]\n"
                    else
                        "\tmov " ^ r ^ ", word ptr [bp " ^ fix_offset inf.parameter_offset ^ "]\n"
                | ENTRY_temporary inf ->
                    "\tlea " ^ r ^ ", " ^ operand_size e ^ " ptr [bp " ^ fix_offset inf.temporary_offset ^ "]\n"
                | _ ->
                    "<error>\n"
            else
                (* non-local operand *)
                match e.entry_info with
                | ENTRY_variable inf ->
                    getAR () ^ 
                    "\tmov " ^ r ^ ", " ^ operand_size e ^ " ptr [si " ^ fix_offset inf.variable_offset ^ "]\n"
                | ENTRY_parameter inf ->
                    if inf.parameter_mode = PASS_BY_VALUE then
                        getAR () ^
                        "\tlea " ^ r ^ ", " ^ operand_size e ^ " ptr [si " ^ fix_offset inf.parameter_offset ^ "]\n"
                    else
                        getAR () ^
                        "\tmov " ^ r ^ ", word ptr [si " ^ fix_offset inf.parameter_offset ^ "]\n"
                | ENTRY_temporary inf ->
                    getAR () ^
                    "n " ^ string_of_int e.entry_scope.sco_nesting ^
                    "\tlea " ^ r ^ ", " ^ operand_size e ^ " ptr [bp " ^ fix_offset inf.temporary_offset ^ "]\n"
                | _ ->
                    "<error>\n"
        end
    | _ -> "<oops>\n"

let store r a =
    match a with
    | Q_funct_res -> "\tmov si, word ptr [bp + 6]\n\tmov word ptr [si], " ^ r ^ "\n"
    | Q_deref e ->  load "di" (Q_entry e) ^ "\tmov " ^ operand_size e ^ " ptr [di], " ^ r ^ "\n" 
    | Q_entry e ->
        begin
            if e.entry_scope.sco_nesting = 2 then
                (* local operand *)
                match e.entry_info with
                | ENTRY_variable inf ->
                    "\tmov " ^ operand_size e ^ " ptr [bp " ^ fix_offset inf.variable_offset ^ "], " ^ r ^ "\n"
                | ENTRY_parameter inf ->
                    if inf.parameter_mode = PASS_BY_VALUE then
                        "\tmov " ^ operand_size e ^ " ptr [bp " ^ fix_offset inf.parameter_offset ^ "], " ^ r ^ "\n"
                    else
                        "\tmov si, word ptr [bp " ^ fix_offset inf.parameter_offset ^ "]\n" ^
                        "\tmov " ^ operand_size e ^ " ptr [si], " ^ r ^ "\n"
                | ENTRY_temporary inf ->
                    "\tmov " ^ operand_size e ^ " ptr [bp " ^ fix_offset inf.temporary_offset ^ "], " ^ r ^ "\n"
                | _ ->
                    "<error>\n"
            else
                (* non-local operand *)
                match e.entry_info with
                | ENTRY_variable inf ->
                    getAR () ^ 
                    "\tmov " ^ operand_size e ^ " ptr [si " ^ fix_offset inf.variable_offset ^ "], " ^ r ^ "\n"
                | ENTRY_parameter inf ->
                    if inf.parameter_mode = PASS_BY_VALUE then
                        getAR () ^
                        "\tmov " ^ operand_size e ^ " ptr [si " ^ fix_offset inf.parameter_offset ^ "], " ^ r ^ "\n"
                    else
                        getAR () ^
                        "\tmov si, word ptr [si " ^ fix_offset inf.parameter_offset ^ "]\n" ^
                        "\tmov " ^ operand_size e ^ " ptr [si], " ^ r ^ "\n"
                | ENTRY_temporary inf ->
                    getAR () ^ 
                    "\tmov " ^ operand_size e ^ " ptr [si " ^ fix_offset inf.temporary_offset ^ "], " ^ r ^ "\n"
                | _ ->
                    "<error>\n"
        end
    | _ -> "<oops>\n"

(* produce a unique assembly label for the beginning of procedure p *)
let name e =
    match e.entry_info with
    | ENTRY_function inf ->
        begin
        (*
        begin
        match inf.function_scope with
        | Some sco -> currentScope := sco
        | None -> internal "dafuq?"
        end;
        *)
        if inf.function_isLibrary
        then
            let name = mapPredefToLib (id_name e.entry_id) in
            "_" ^ name
        else
            begin
            let lbl = 
            match inf.function_label with
            | Some c ->
                "_" ^ id_name e.entry_id ^ "_" ^ string_of_int c
            | None ->
                incnat ();
                inf.function_label <- Some !nat;
                "_" ^ id_name e.entry_id ^ "_" ^ string_of_int !nat
            in
            if inf.function_isMain then !main <- lbl;
            lbl
            end
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
            "<error>\n"
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
            "<error>\n"
        end
    | Q_op (op,x,y,z) ->
        begin
        match op with
        | "+" ->
            (*
            match x, y, z with
            | Q_entry x1, Q_entry y1, Q_entry z1
            | Q_entry x1, Q_int y1, Q_entry z1
            | Q_int x1, Q_entry y1, Q_entry z1 ->
                (x, y, z)
            | Q_deref x1, Q_entry y1, Q_entry z1
            | Q_deref x1, Q_int y1, Q_entry z1 ->
                (x1, y, z)
            | Q_entry x1, Q_deref y1, Q_entry z1
            | Q_int x1, Q_deref y1, Q_entry z1 ->
                (x, y1, z)
            | _ -> (Q_none, Q_none, Q_none)
            *)
            load "ax" x ^
            load "dx" y ^
            "\tadd ax, dx\n" ^
            store "ax" z
        | "-" ->
            begin
            match y with
            | Q_dash ->
                load "ax" (Q_int 0) ^
                load "dx" x ^
                "\tsub ax, dx\n" ^
                store "ax" z
            | _ ->
                load "ax" x ^
                load "dx" y ^
                "\tsub ax, dx\n" ^
                store "ax" z
            end
        | "*" ->
            begin
            match x, y, z with
            | Q_entry x1, Q_entry y1, Q_entry z1 ->
                load "ax" x ^
                load "cx" y ^
                "\timul cx\n" ^
                store "ax" z
            | Q_deref x1, Q_deref y1, Q_entry z1 ->
                load "ax" x ^
                load "cx" y ^
                "\timul cx\n" ^
                store "ax" z
            | Q_entry x1, Q_int y1, Q_entry z1 ->
                load "ax" x ^
                load "cx" y ^
                "\timul cx\n" ^
                store "ax" z
            | Q_int x1, Q_entry y1, Q_entry z1 ->
                load "ax" x ^
                load "cx" y ^
                "\timul cx\n" ^
                store "ax" z
            | _ -> "<error>\n"
            end
        | "/" ->
            begin
            match x, y, z with
            | Q_entry x1, Q_entry y1, Q_entry z1 ->
                load "ax" x ^
                "\tcwd\n" ^
                load "cx" y ^
                "\tidiv cx\n" ^
                store "ax" z
            | Q_entry x1, Q_int y1, Q_entry z1 ->
                load "ax" x ^
                "\tcwd\n" ^
                load "cx" y ^
                "\tidiv cx\n" ^
                store "ax" z
            | Q_int x1, Q_entry y1, Q_entry z1 ->
                load "ax" x ^
                "\tcwd\n" ^
                load "cx" y ^
                "\tidiv cx\n" ^
                store "ax" z
            | _ -> "<error>\n"
            end
        | "%" ->
            begin
            match x, y, z with
            | Q_entry x1, Q_entry y1, Q_entry z1 ->
                load "ax" x ^
                "\tcwd\n" ^
                load "cx" y ^
                "\tidiv cx\n" ^
                store "dx" z
            | Q_entry x1, Q_int y1, Q_entry z1 ->
                load "ax" x ^
                "\tcwd\n" ^
                load "cx" y ^
                "\tidiv cx\n" ^
                store "dx" z
            | Q_int x1, Q_entry y1, Q_entry z1 ->
                load "ax" x ^
                "\tcwd\n" ^
                load "cx" y ^
                "\tidiv cx\n" ^
                store "dx" z
            | _ -> "<error>\n"
            end
        | "&&" ->
            begin
                load "al" x ^
                load "dl" y ^
                "\tand al, dl\n" ^
                store "al" z
            end
        | "||" ->
            begin
                load "al" x ^
                load "dl" y ^
                "\tor al, dl\n" ^
                store "al" z
            end
        | _ -> "<error>\n"
        end
    | Q_assign (x,z) ->
        begin
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
        | Q_deref z1 ->
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
            load "ax" x ^
            store "ax" z
        | _ -> "<error>\n"
        end
    | Q_array (x,y,z) ->
        begin
        match x, y, z with
        | Q_entry x1, Q_entry y1, Q_entry z1 ->
            load "ax" y ^
            "\tmov cx, " ^ operand_size x1 ^ "\n" ^
            "\timul cx\n" ^
            loadAddr "cx" x ^
            "\tadd ax, cx\n" ^
            store "ax" z
        | Q_entry x1, Q_int y1, Q_entry z1 ->
            load "ax" y ^
            "\tmov cx, " ^ operand_size x1 ^ "\n" ^
            "\timul cx\n" ^
            loadAddr "cx" x ^
            "\tadd ax, cx\n" ^
            store "ax" z
        | Q_deref x1, Q_entry y1, Q_entry z1 ->
            load "ax" y ^
            "\tmov cx, " ^ operand_size x1 ^ "\n" ^
            "\timul cx\n" ^
            load "cx" (Q_entry x1) ^
            "\tadd ax, cx\n" ^
            store "ax" z
        | Q_deref x1, Q_int y1, Q_entry z1 ->
            load "ax" y ^
            "\tmov cx, " ^ operand_size x1 ^ "\n" ^
            "\timul cx\n" ^
            load "cx" (Q_entry x1) ^
            "\tadd ax, cx\n" ^
            store "ax" z
        | _ ->
            "<error>\n"
        end
    | Q_relop (op,x,y,z) ->
        begin
        let instr = 
            match op with
            | "==" -> "je"
            | "!=" -> "jne"
            | "<" -> "jl"
            | ">" -> "jg"
            | "<=" -> "jle"
            | ">=" -> "jge"
            | _ -> "<error>"
        in
        begin
        match x, y, z with
        | Q_deref x1, Q_deref y1, Q_int z1 ->
            load "ax" x ^
            load "dx" y ^
            "\tcmp ax, dx\n" ^
            "\t" ^ instr ^ " @" ^ string_of_int z1 ^ "\n"
        | Q_deref x1, _, Q_int z1 ->
            load "ax" (Q_entry x1) ^
            load "dx" y ^
            "\t cmp ax, dx\n" ^
            "\t" ^ instr ^ " @" ^ string_of_int z1 ^ "\n"
        | _, Q_deref y1, Q_int z1 ->
            load "ax" x ^
            load "dx" (Q_entry y1) ^
            "\tcmp ax, dx\n" ^
            "\t" ^ instr ^ " @" ^ string_of_int z1 ^ "\n"
        | _, _, Q_int z1 ->
            load "ax" x ^
            load "dx" y ^
            "\tcmp ax, dx\n" ^
            "\t" ^ instr ^ " @" ^ string_of_int z1 ^ "\n"
        | _, _, _ ->
            "<error>\n"
        end
        end
    | Q_ifb (x,z) ->
        begin
        match x, z with
        | _, Q_int z1 ->
        (* | Q_entry x1, Q_int z1 -> *)
            load "al" x ^
            "\tor al, al\n" ^
            "\tjnz @" ^ string_of_int z1 ^ "\n"
        | _ ->
            "<error>\n"
        end
    | Q_jump z ->
        begin
        match z with
        | Q_int i ->
            "\tjmp @" ^ string_of_int i ^ "\n"
        | _ ->
            "<error>\n"
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
            "<error>\n"
        end
    | Q_par (x,m) ->
        begin
        match x, m with
        | Q_int x1, Q_pass_mode mode ->
            load "ax" x ^
            "\tpush ax\n"
        | Q_bool x1, Q_pass_mode mode ->
            load "al" x ^
            "\tsub sp, 1\n" ^
            "\tmov si, sp\n" ^
            "\tmov byte ptr [si], al\n"
        | Q_char x1, Q_pass_mode mode ->
            load "al" x ^
            "\tsub sp, 1\n" ^
            "\tmov si, sp\n" ^
            "\tmov byte ptr [si], al\n"
        | Q_string s, Q_pass_mode mode ->
            loadAddr "si" x ^
            "\tpush si\n"
        | Q_deref x1, Q_pass_mode mode
        | Q_entry x1, Q_pass_mode mode ->
            begin
            match mode with
            | V -> 
                let sz =
                    match x1.entry_info with
                    | ENTRY_variable inf -> sizeOfType inf.variable_type
                    | ENTRY_parameter inf -> sizeOfType inf.parameter_type
                    | ENTRY_temporary inf -> sizeOfType inf.temporary_type
                    | _ -> 2
                in 
                if (sz = 1)
                then
                    load "al" x ^
                    "\tsub sp, 1\n" ^
                    "\tmov si, sp\n" ^
                    "\tmov byte ptr [si], al\n"
                else
                    load "ax" x ^
                    "\tpush ax\n"
            | R ->
                loadAddr "si" x ^
                "\tpush si\n"
            | RET ->
                loadAddr "si" x ^
                "\tpush si\n"
            end
        | _ ->
            "<error>\n"
        end
    | Q_ret ->
        "\tjmp " ^ !curr ^ "\n"

let generate icode =
    let middle = List.fold_left (^) "" (List.map transform icode) in
    let top = header () in
    let bottom = footer () in
    top ^ middle ^ bottom
