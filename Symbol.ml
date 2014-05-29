open Identifier
open Error
open Types

(* Functor building an implementation of the Hashtbl structure
 * on data type : id, with comparer : ==, and hash function : Hashtbl.hash
 * Output : a module parametrized by an input module
 * Input :
 * module type Hashedtype = sig
 *                            type t
 *                            val equal : t -> t -> bool
 *                            val hash : t -> int
 *                          end
 *)
module H = Hashtbl.Make (
  struct
    type t = id
    let equal = (==)
    let hash = Hashtbl.hash
  end
)


(* change to true for parser output *)
let debug = true;;


(* type definition for parameter passing *)
type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE

(* type definition for parameter status *)
type param_status =
  | PARDEF_COMPLETE         (* Πλήρης ορισμός  *) (* after endFunctionHeader *)
  | PARDEF_DEFINE           (* Εν μέσω ορισμού *) (* initial *)
  | PARDEF_CHECK            (* Εν μέσω ελέγχου *) (* when redeclared after forwarded *)

(* the value of a Constant stored in the Symbol Table *)
type const_val = CONST_none
               | CONST_int of int
               | CONST_bool of bool
               | CONST_REAL of float
               | CONST_char of char
               (* | CONST_string of string *)

(* type definition for scopes *)
type scope = {
  sco_parent : scope option;            (* parent scope is optional *)
  sco_nesting : int;                    (* nesting level *)
  mutable sco_entries : entry list;     (* list of entries in this scope *)
  mutable sco_negofs : int              (* negative offset in this scope *)
}

and variable_info = {                         (******* Μεταβλητή *******)
  variable_type   : Types.typ;                (* Τύπος                 *)
  variable_offset : int                       (* Offset στο Ε.Δ.       *)
}

and function_info = {                         (******* Συνάρτηση *******)
  mutable function_isForward : bool;          (* Δήλωση forward        *)
  mutable function_paramlist : entry list;    (* Λίστα παραμέτρων      *)
  mutable function_redeflist : entry list;    (* Λίστα παραμέτρων (2η) *)
  mutable function_result    : Types.typ;     (* Τύπος αποτελέσματος   *)
  mutable function_pstatus   : param_status;  (* Κατάσταση παραμέτρων  *)
  mutable function_initquad  : int;           (* Αρχική τετράδα        *)
  mutable function_scope     : scope option;  (* Εμβέλεια συνάρτησης   *)
  mutable function_label     : int option;    (* Ετικέτα Τελικού Κώδ.  *)
  mutable function_isMain    : bool;          (* Main Συνάρτηση        *)
  mutable function_isLibrary : bool           (* Συνάρτηση Βιβλιοθήκης *)
}

and parameter_info = {                        (****** Παράμετρος *******)
  parameter_type           : Types.typ;       (* Τύπος                 *)
  mutable parameter_offset : int;             (* Offset στο Ε.Δ.       *)
  parameter_mode           : pass_mode        (* Τρόπος περάσματος     *)
}

and temporary_info = {                        (** Προσωρινή μεταβλητή **)
  temporary_type   : Types.typ;               (* Τύπος                 *)
  temporary_offset : int                      (* Offset στο Ε.Δ.       *)
}

(* Constant Info *)
and constant_info = {
  constant_type : Types.typ;
  constant_value : const_val
}

(* SymbolTable entry information datatype *)
and entry_info = ENTRY_none
               | ENTRY_variable of variable_info
               | ENTRY_function of function_info
               | ENTRY_parameter of parameter_info
               | ENTRY_temporary of temporary_info
               | ENTRY_constant of constant_info

(* Symbol Table entry *)
and entry = {
  entry_id    : Identifier.id;
  entry_scope : scope;
  entry_info  : entry_info
}

type lookup_type = LOOKUP_CURRENT_SCOPE | LOOKUP_ALL_SCOPES

let start_positive_offset = 8   (* Αρχικό θετικό offset στο Ε.Δ.   *)

let start_negative_offset = 0   (* Αρχικό αρνητικό offset στο Ε.Δ. *)

(* global scope *)
let the_outer_scope = {
  sco_parent = None;
  sco_nesting = 0;
  sco_entries = [];
  sco_negofs = start_negative_offset
}

(* an ENTRY_none entry, used for errors *)
let no_entry id = {
  entry_id = id;
  entry_scope = the_outer_scope;
  entry_info = ENTRY_none
}

let currentScope = ref the_outer_scope  (* Τρέχουσα εμβέλεια *)

let quadNext = ref 1                    (* Αριθμός επόμενης τετράδας *)

let tempNumber = ref 1                  (* Αρίθμηση των temporaries  *)

(* H is our special HashTable => SymbolTable *)
let tab = ref (H.create 0)

(* Initialize the Symbol Table *)
let initSymbolTable size =
   tab := H.create size;
   currentScope := the_outer_scope

(* Add a new scope with an empty list of entries and
 * increased nesting level and set it as current scope *)
let openScope () =
  Printf.printf " ---> A scope has been opened\n";
  let sco = {
    sco_parent = Some !currentScope;
    sco_nesting = !currentScope.sco_nesting + 1;
    sco_entries = [];
    sco_negofs = start_negative_offset
  } in
  currentScope := sco

(* Close the current scope, remove from HashTable
 * all scope entries, and set scope father as current scope *)
let closeScope () =
  Printf.printf " <--- A scope has been closed\n";
  let sco = !currentScope in
  let manyentry e = H.remove !tab e.entry_id in
  (* apply manyentry to every item in sco_entries list *)
  List.iter manyentry sco.sco_entries;
  match sco.sco_parent with
  | Some scp ->
      currentScope := scp
  | None ->
      internal "cannot close the outer scope!"

(* declare a new exception *)
exception Failure_NewEntry of entry

(* Add a new entry in the Symbol Table and return it.
 * id is after id_make, inf is entry_info
 * err = true means search the ST for that entry
 * err = false means do nothing
 *)
let newEntry id inf err =
  try
    if err then begin
      (* search the ST *)
      try
        let e = H.find !tab id in
        (* if a same entry exists in the current scope, error *)
        if e.entry_scope.sco_nesting = !currentScope.sco_nesting then
          (* raise an exception caught 20 lines later *)
          raise (Failure_NewEntry e)
      with Not_found ->
        (* do nothing *)
        ()
    end;
    (* IF not searched OR searched but not found THEN : *)
    (* make the new entry *)
    let e = {
      entry_id = id;
      entry_scope = !currentScope;
      entry_info = inf
    } in
    (* add the new entry to the ST *)
    H.add !tab id e;
    (* add the new entry to the current scope entry list *)
    !currentScope.sco_entries <- e :: !currentScope.sco_entries;
    (* return the new entry *)
    e
  with Failure_NewEntry e ->
    (* report an error, since that entry already exists *)
    error "duplicate identifier %a" pretty_id id;
    (* return that entry *)
    e

(* Check if entry exists in Symbol Table
 * return entry if found, else raise Not_found/Exit
 * err = true handles not_found with an error message
 * err = false does nothing (used for function forward definitions) *)
let lookupEntry id how err =
  let csc = !currentScope in
  let lookup () =
    match how with
    | LOOKUP_CURRENT_SCOPE ->
        let e = H.find !tab id in
        if e.entry_scope.sco_nesting = csc.sco_nesting then
          (* if found in current scope, return it *)
          e
        else
          (* manually raise Not_found *)
          raise Not_found
    | LOOKUP_ALL_SCOPES ->
        H.find !tab id in
  if err then
    try
      lookup ()
    with Not_found ->
      (* report an error because entry is not in Symbol table *)
      error "unknown identifier %a (first occurrence)" pretty_id id;
      (* put it in, so we don't see more errors *)
      H.add !tab id (no_entry id);
      raise Exit
  else
    lookup ()

(* Add a new variable to the Symbol Table
 * name is id (after id_make), type is typ 
 * err = true means search the ST for possible duplicate
 * call newEntry *)
let newVariable id typ err =
  !currentScope.sco_negofs <- !currentScope.sco_negofs - sizeOfType typ;
  let inf = {
    variable_type = typ;
    variable_offset = !currentScope.sco_negofs
  } in
  newEntry id (ENTRY_variable inf) err

(* Add a new constant to the Symbol Table
 * name is id (after id_make), type is typ and value is v
 * err = true means search the ST for possible duplicate
 * call newEntry *)
let newConstant id typ v err =
  let inf = {
    constant_type = typ;
    constant_value = v;
  } in
  newEntry id (ENTRY_constant inf) err

(* Make a new temporary variable
 * call newEntry *)
let newTemporary typ =
  let id = id_make ("$" ^ string_of_int !tempNumber) in
  !currentScope.sco_negofs <- !currentScope.sco_negofs - sizeOfType typ;
  let inf = {
    temporary_type = typ;
    temporary_offset = !currentScope.sco_negofs
  } in
  incr tempNumber;
  newEntry id (ENTRY_temporary inf) false

(* Add a new function to the Symbol Table
 * err should be true
 * if function not exists in ST then call newEntry
 * else if function was forwarded, start parameter checking *)
let newFunction id err isLib =
  try
    (* search the current scope for another entry with the same name *)
    let e = lookupEntry id LOOKUP_CURRENT_SCOPE false in
    match e.entry_info with
    (* if found and is a forward function definiton
     * it means we're about to see the function body *)
    | ENTRY_function inf when inf.function_isForward ->
        if not isLib then Printf.printf "this function '%s' has already be defined\n" (id_name id);
        (* function is no more forwarded *)
        (* FIXME is it really? *)
        inf.function_isForward <- false;
        (* parameters have been defined and now should be checked *)
        inf.function_pstatus <- PARDEF_CHECK;
        (* copy all parameter entries to redeflist *)
        (* FIXME deep copy? *)
        inf.function_redeflist <- inf.function_paramlist;
        (* empty the function paramlist *)
        inf.function_paramlist <- [];
        (* return the function entry *)
        e
    | _ ->
        (* if found but is not a forwarded function, error *)
        if err then
          error "duplicate identifier: %a" pretty_id id;
          raise Exit
  (* if not found in current scope *)
  with Not_found ->
    let inf = {
      function_isForward = false;
      function_paramlist = [];
      function_redeflist = [];
      function_result = TYPE_none;
      function_pstatus = PARDEF_DEFINE;
      function_initquad = 0;
      function_scope = None;
      function_label = None;
      function_isMain = false;
      function_isLibrary = isLib
    } in
    if not isLib then Printf.printf "this function '%s' is a new one\n" (id_name id);
    (* register a new function entry in the Symbol Table *)
    newEntry id (ENTRY_function inf) false

(* Add a new function parameter
 * err should be true
 * if function par status is PARDEF_DEFINE, add par to ST
 * if function par status is PARDEF_CHECK, check for compatibility
 * with forward definition and add par to ST
 * call newEntry *)
let newParameter id typ mode f err =
  match f.entry_info with
  (* f.entry_info must be ENTRY_function *)
  | ENTRY_function inf -> begin
      (* match function parameter status with ... *)
      match inf.function_pstatus with
      (* while defining a new function *)
      | PARDEF_DEFINE ->
          (* new parameter_info *)
          let inf_p = {
            parameter_type = typ;
            parameter_offset = 0; (* fixed later *)
            parameter_mode = mode
          } in
          (* register a new entry for the parameter *)
          let e = newEntry id (ENTRY_parameter inf_p) err in
          (* append the entry to the functions parameter entry list *)
          inf.function_paramlist <- e :: inf.function_paramlist;
          (* return the entry *)
          e
      (* while checking the function parameters *)
      | PARDEF_CHECK -> begin
          (* check that par is same as in function forward definition *)
          match inf.function_redeflist with
          | p :: ps -> begin
              Printf.printf "will now check function parameter '%s'...\n" (id_name p.entry_id);
              (* ps is the list of rest par entries *)
              inf.function_redeflist <- ps;
              (* p is the first parameter, to be matched with par id *)
              match p.entry_info with
              | ENTRY_parameter pinf ->
                  (* type matching *)
                  if not (equalType pinf.parameter_type typ) then
                      begin
                      error "Parameter type mismatch in redeclaration of function %a" pretty_id f.entry_id;
                      p
                      end
                  (* passing mode matching *)
                  else if pinf.parameter_mode != mode then
                      begin
                      error "Parameter passing mode mismatch in redeclaration of function %a" pretty_id f.entry_id;
                      p
                      end
                  (* name matching *)
                  else if p.entry_id != id then
                      begin
                      error "Parameter name mismatch in redeclaration of function %a" pretty_id f.entry_id;
                      p
                      end
                  (* if all ok, add the parameter entry to the Hashtable *)
                  else
                      (*
                      (* FIXME why? *)
                      H.add !tab id p;
                      (* return the parameter entry *)
                      p
                      *)
                      begin
                      (* remove the old entry *)
                      ignore (H.remove !tab id);
                      (* register a new entry for the parameter *)
                      let e = newEntry id (ENTRY_parameter pinf) err in
                      (* append the entry to the functions parameter entry list *)
                      inf.function_paramlist <- inf.function_paramlist @ [e];
                      (* return the entry *)
                      e
                      end
              (* should never reach *)
              | _ ->
                  internal "I found a parameter that is not a parameter!";
                  raise Exit
            end
          (* trying to match more parameters than actual definition *)
          | [] ->
              error "More parameters than expected in redeclaration \
                     of function %a" pretty_id f.entry_id;
              raise Exit
        end
      (* after full definition *)
      | PARDEF_COMPLETE ->
          internal "Cannot add a parameter to an already defined function";
          raise Exit
    end
  (* should never reach *)
  | _ ->
      internal "Cannot add a parameter to a non-function";
      raise Exit

(* set a function to be forwarded *)
let forwardFunction e =
  match e.entry_info with
  | ENTRY_function inf ->
      inf.function_isForward <- true
  | _ ->
      internal "Cannot make a non-function forward"

(* finish function checking
 * typ is function return type
 * after function definition or after function check *)
let endFunctionHeader e typ =
  match e.entry_info with
  (* e.entry_info must be ENTRY_function *)
  | ENTRY_function inf ->
      begin
        match inf.function_pstatus with
        (* error *)
        | PARDEF_COMPLETE ->
            internal "Cannot end parameters in an already defined function"
        (* after function definition, set return type and fix parameter offsets *)
        | PARDEF_DEFINE ->
            (* set the return type *)
            inf.function_result <- typ;
            let offset = ref start_positive_offset in
            (* a function that sets the offset in every function parameter *)
            let fix_offset e =
              match e.entry_info with
              | ENTRY_parameter inf ->
                  inf.parameter_offset <- !offset;
                  let size =
                    match inf.parameter_mode with
                    | PASS_BY_VALUE     -> sizeOfType inf.parameter_type
                    | PASS_BY_REFERENCE -> 2 in
                  offset := !offset + size
              | _ ->
                  internal "Cannot fix offset to a non parameter" in
            (* fix all parameter offsets *)
            List.iter fix_offset inf.function_paramlist;
            (* reverse the parameter list *)
            inf.function_paramlist <- List.rev inf.function_paramlist
        (* after function parameter checking *)
        | PARDEF_CHECK ->
            (* if there are parameters left *)
            if inf.function_redeflist <> [] then
              error "Fewer parameters than expected in redeclaration \
                     of function %a" pretty_id e.entry_id;
            (* check return type *)
            if not (equalType inf.function_result typ) then
              error "Result type mismatch in redeclaration of function %a"
                    pretty_id e.entry_id;
      end;
      (* set function parameter status as complete *)
      inf.function_pstatus <- PARDEF_COMPLETE
  (* should never reach *)
  | _ ->
      internal "Cannot end parameters in a non-function"
