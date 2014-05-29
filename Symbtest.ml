open Format

open Identifier
open Types
open Symbol

let show_offsets = true;;

let rec string_of_type typ =
    match typ with
    | TYPE_none -> "<undefined>"
    | TYPE_int -> "int"
    | TYPE_bool -> "bool"
    | TYPE_char -> "char"
    | TYPE_REAL -> "REAL"
    | TYPE_array (et,sz) ->
        string_of_type et ^
        if sz > 0 then
            "[" ^ string_of_int sz ^ "]"
        else
            "[]"
    | TYPE_proc -> "PROC"

let string_of_const inf =
    " CONST " ^ string_of_type inf.constant_type ^ " = " ^
    match inf.constant_value with
    | CONST_none ->
        "NONE"
    | CONST_int a ->
        string_of_int a 
    | CONST_bool a ->
        if a then "true" else "false"
    | CONST_REAL a ->
        string_of_float a
    | CONST_char a ->
        String.make 1 a
    (*
    | CONST_string a ->
          a
    *)

let string_of_mode mode =
    match mode with
    | PASS_BY_REFERENCE -> "reference "
    | _ -> ""

let string_of_parameter e =
    match e.entry_info with
    | ENTRY_parameter inf ->
        (* print pass_mode name and type *)
        string_of_mode inf.parameter_mode
        ^ id_name e.entry_id
        ^ " : "
        ^ string_of_type inf.parameter_type
        ^ ", "
    | _ ->
        "<invalid>"

let string_of_function inf =
    (* print every parameter *)
    let params = List.fold_left (^) "" (List.map string_of_parameter inf.function_paramlist) in
    (* print every parameter *)
    let typ = string_of_type inf.function_result in
    let size = 
        match inf.function_scope with
        | Some sco -> " with size : " ^ string_of_int (-sco.sco_negofs)
        | _ -> " with size : -1"
    in
    "(" ^ params ^ "[eop]) : " ^ typ ^ size

let print_entry e =
    let name = id_name e.entry_id in
    let info =
        match e.entry_info with
        | ENTRY_none ->
            string_of_type TYPE_none
        | ENTRY_variable inf ->
            string_of_type inf.variable_type
            ^
            if show_offsets
                then "[" ^ string_of_int inf.variable_offset ^ "]"
                else ""
        | ENTRY_constant inf ->
            string_of_const inf
        | ENTRY_function inf ->
            if inf.function_isLibrary then "" else string_of_function inf
        | ENTRY_parameter inf ->
            string_of_type inf.parameter_type
            ^
            if show_offsets
                then "[" ^ string_of_int inf.parameter_offset ^ "]" 
                else ""
        | ENTRY_temporary inf ->
            string_of_type inf.temporary_type
            ^
            if show_offsets
                then "[" ^ string_of_int inf.temporary_offset ^ "]" 
                else ""
    in
    name ^ " " ^ info ^ ", "

let print_scope sco =
    List.fold_left (^) "" (List.map print_entry sco.sco_entries)

let rec walk sco =
    let par = match sco.sco_parent with
        | Some scopar -> walk scopar
        | None -> "GlobalScope of the House Compilers\n"
    in
    print_scope sco ^ "[eos]\n\tson of:\n" ^ par

let printSymbolTable () =
    printf "\n----------------------------------------\n";
    printf "%s" (walk !currentScope);
    printf "----------------------------------------\n\n"


(* OLD 

(* convert type to string *)
let rec pretty_typ ppf typ =
  match typ with
  | TYPE_none ->
      fprintf ppf "<undefined>"
  | TYPE_int ->
      fprintf ppf "int"
  | TYPE_bool ->
      fprintf ppf "bool"
  | TYPE_char ->
      fprintf ppf "char"
  | TYPE_REAL ->
      fprintf ppf "REAL"
  | TYPE_array (et, sz) ->
      pretty_typ ppf et;
      if sz > 0 then
        fprintf ppf "[%d]" sz
      else
        fprintf ppf "[]"
  | TYPE_proc ->
      fprintf ppf "PROC"

(* convert pass mode to string *)
let pretty_mode ppf mode =
  match mode with
  | PASS_BY_REFERENCE ->
      fprintf ppf "reference "
  | _ ->
      ()

(* print the Symbol Table *)
let printSymbolTable () =
  (* walk through the scope printing info about every entry, then walk the parent scope *)
  let rec walk ppf scp =
    (* if currently not in outer scope *)
    if scp.sco_nesting <> 0 then
    begin
      fprintf ppf "scope: ";
      (* a function to print entry info *)
      let entry ppf e =
        (* print the entry name *)
        fprintf ppf "%a" pretty_id e.entry_id;
        (* and print more info *)
        match e.entry_info with
        | ENTRY_none ->
            fprintf ppf ":%a " pretty_typ TYPE_none;
            fprintf ppf "<none>"
        | ENTRY_variable inf ->
            fprintf ppf ":%a " pretty_typ inf.variable_type;
            if show_offsets then
              fprintf ppf "[%d]" inf.variable_offset
        | ENTRY_constant inf ->
            let print_const = function
              | CONST_none ->
                    fprintf ppf " CONST %a = NONE"
                    pretty_typ inf.constant_type
              | CONST_int (a) ->
                    fprintf ppf " CONST %a = %i"
                    pretty_typ inf.constant_type
                    a 
              | CONST_bool (a) ->
                    fprintf ppf " CONST %a = %s"
                    pretty_typ inf.constant_type
                    ( if (a) then "true" else "false")
              | CONST_REAL (a) ->
                    fprintf ppf " CONST %a = REAL"
                    pretty_typ inf.constant_type
              | CONST_char (a) ->
                    fprintf ppf " CONST %a = %c"
                    pretty_typ inf.constant_type
                    a
              (*
              | CONST_string (a) ->
                    fprintf ppf " CONST %a = %s"
                    pretty_typ inf.constant_type
                    a
              *)
            in print_const inf.constant_value
        | ENTRY_function inf ->
            (* if function, print every parameter *)
            let param ppf e =
              match e.entry_info with
                | ENTRY_parameter inf ->
                   (* print pass_mode name and type *)
                   fprintf ppf "%a%a : %a"
                      pretty_mode inf.parameter_mode
                      pretty_id e.entry_id
                      pretty_typ inf.parameter_type
                | _ ->
                    fprintf ppf "<invalid>" in
            let rec params ppf ps =
              match ps with
              | [p] ->
                  fprintf ppf "%a" param p
              | p :: ps ->
                  fprintf ppf "%a; %a" param p params ps;
              | [] ->
                  () in
            (* print every parameter *)
            fprintf ppf "(%a) : %a"
              params inf.function_paramlist
              pretty_typ inf.function_result;
            begin
            match inf.function_scope with
            | Some sco ->
                fprintf ppf " size : %d" (-sco.sco_negofs)
            | _ ->
                fprintf ppf " size : -1"
            end
        | ENTRY_parameter inf ->
            fprintf ppf ":%a " pretty_typ inf.parameter_type;
            if show_offsets then
              fprintf ppf "[%d]" inf.parameter_offset
        | ENTRY_temporary inf ->
            fprintf ppf ":%a " pretty_typ inf.temporary_type;
            if show_offsets then
              fprintf ppf "[%d]" inf.temporary_offset in
      (* a function to print many entries info *)
      let rec entries ppf es =
        match es with
          | [e] ->
              fprintf ppf "%a" entry e
          | e :: es ->
              fprintf ppf "%a, %a" entry e entries es;
          | [] ->
              () in
      (* print the scope entries and then walk the parent scope *)
      match scp.sco_parent with
      | Some scpar ->
          fprintf ppf "%a\n%a"
            (* print info about all entries in this scope *)
            entries scp.sco_entries
            (* walk the parent scope *)
            walk scpar
      | None ->
          (* cannot reach since we've checked sco_nesting != 0 *)
          fprintf ppf "<impossible>\n"
    end 
    in
  (* a function to print the whole Symbol Table *)
  let scope ppf scp =
    if scp.sco_nesting == 0 then
      fprintf ppf "no scope\n"
    else
      (* walk the scope, then its parent etc... *)
      walk ppf scp
  in
  (* do the actual shit! walk the currentScope, then its parent etc... *)
  printf "%a----------------------------------------\n"
    scope !currentScope

*)

(*

(* Κύριο πρόγραμμα επίδειξης του πίνακα συμβόλων *)

(* Ακολουθεί ο κώδικας ενός προγράμματος Alan που χρησιμοποιείται
   για τον έλεγχο του πίνακα συμβόλων.

   p () : proc -- this is the program header

   s1 : byte; s2 : byte; s3 : byte;
   i1 : int; i2 : int;

   pr (p1 : int, p2 : int, p3 : reference byte) : proc
   b1 : byte;
   i1 : int;
   { -- of procedure pr
          -- access s1, i2, i1
          -- make 2 new temporaries
   } -- of procedure pr

   f (x : int; reference y : byte) : int
   { -- of function f
          -- ... whatever ...
   } -- of function f

   { -- of program p
          -- make 2 new temporaries
   } -- of program p
*)

let main =

   initSymbolTable 256;

   printSymbolTable ();

   (* SOURCE : p () : proc -- this is the program header *)

   openScope ();

   printSymbolTable ();

   (* SOURCE : s1 : byte; s2 : byte; s3 : byte; *)

   let s1 = newVariable (id_make "s1") TYPE_byte true in
   let s2 = newVariable (id_make "s2") TYPE_byte true in
   let s3 = newVariable (id_make "s3") TYPE_byte true in
   ignore s1; ignore s2; ignore s3;

   printSymbolTable ();

   (* SOURCE : i1 : int; i2 : int; *)

   let i1 = newVariable (id_make "i1") TYPE_int true in
   let i2 = newVariable (id_make "i2") TYPE_int true in
   ignore i1; ignore i2;

   printSymbolTable ();

   (* SOURCE : pr (... *)

   let p = newFunction (id_make "pr") true in
   openScope ();

   printSymbolTable ();

   (* SOURCE : p1 : int, p2 : int, p3 : reference byte) : proc *)

   let p1 = newParameter (id_make "p1") TYPE_int PASS_BY_VALUE p true in
   let p2 = newParameter (id_make "p2") TYPE_int PASS_BY_VALUE p true in
   let p3 = newParameter (id_make "p3") TYPE_byte PASS_BY_REFERENCE p true in
   endFunctionHeader p TYPE_proc;
   ignore p1; ignore p2; ignore p3;

   printSymbolTable ();

   (* SOURCE : b1 : byte; *)

   let b1 = newVariable (id_make "b1") TYPE_byte true in
   ignore b1;

   (* SOURCE : i1 : int; *)

   let i1 = newVariable (id_make "i1") TYPE_int true in
   ignore i1;

   printSymbolTable ();

   (* SOURCE : (* access s1, i2, i1 *) *)

   let s1 = lookupEntry (id_make "s1") LOOKUP_ALL_SCOPES true in
   let i2 = lookupEntry (id_make "i2") LOOKUP_ALL_SCOPES true in
   let i1 = lookupEntry (id_make "i1") LOOKUP_ALL_SCOPES true in
   ignore s1; ignore i2; ignore i1;

   (* SOURCE : (* make 2 new temporaries *) *)

   let t1 = newTemporary TYPE_int in
   let t2 = newTemporary TYPE_byte in
   ignore t1; ignore t2;

   printSymbolTable ();

   (* SOURCE : } -- of procedure pr *)

   closeScope ();

   printSymbolTable ();

   (* SOURCE : f (... *)

   let p = newFunction (id_make "f") true in
   openScope ();

   printSymbolTable ();

   (* SOURCE : x : int; reference y : byte) : int *)

   let x = newParameter (id_make "x") TYPE_int PASS_BY_VALUE p true in
   let y = newParameter (id_make "y") TYPE_byte PASS_BY_REFERENCE p true in
   endFunctionHeader p TYPE_int;
   ignore x; ignore y;

   printSymbolTable ();

   (* SOURCE : } -- of procedure f *)

   closeScope ();

   printSymbolTable ();

   (* SOURCE : (* make 2 new temporaries *) *)

   let t1 = newTemporary TYPE_int in
   let t2 = newTemporary TYPE_int in
   ignore t1; ignore t2;

   printSymbolTable ();

   (* SOURCE : } -- of program p *)

   closeScope ();

   printSymbolTable ()

*)
