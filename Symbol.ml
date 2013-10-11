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

(* type definition for parameter passing *)
type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE

type param_status =
  | PARDEF_COMPLETE
  | PARDEF_DEFINE
  | PARDEF_CHECK

(* type definition for scopes *)
type scope = {
  sco_parent : scope option;    (* parent scope is optional *)
  sco_nesting : int;            (* nesting level *)
  mutable sco_entries : entry list;
  mutable sco_negofs : int
}

and variable_info = {
  variable_type   : Types.typ;
  variable_offset : int
}

and function_info = {
  mutable function_isForward : bool;
  mutable function_paramlist : entry list;
  mutable function_redeflist : entry list;
  mutable function_result    : Types.typ;
  mutable function_pstatus   : param_status;
  mutable function_initquad  : int
}

and parameter_info = {
  parameter_type           : Types.typ;
  mutable parameter_offset : int;
  parameter_mode           : pass_mode
}

and temporary_info = {
  temporary_type   : Types.typ;
  temporary_offset : int
}

and entry_info = ENTRY_none
               | ENTRY_variable of variable_info
               | ENTRY_function of function_info
               | ENTRY_parameter of parameter_info
               | ENTRY_temporary of temporary_info

and entry = {
  entry_id    : Identifier.id;
  entry_scope : scope;
  entry_info  : entry_info
}

type lookup_type = LOOKUP_CURRENT_SCOPE | LOOKUP_ALL_SCOPES

let start_positive_offset = 8
let start_negative_offset = 0

(* global scope *)
let the_outer_scope = {
  sco_parent = None;
  sco_nesting = 0;
  sco_entries = [];
  sco_negofs = start_negative_offset
}

(* make ENTRY_none entry, used for errors *)
let no_entry id = {
  entry_id = id;
  entry_scope = the_outer_scope;
  entry_info = ENTRY_none
}

let currentScope = ref the_outer_scope
let quadNext = ref 1
let tempNumber = ref 1

(* H is our special HashTable *)
let tab = ref (H.create 0)

(* Initialize the Symbol Table *)
let initSymbolTable size =
   tab := H.create size;
   currentScope := the_outer_scope

(* Open a new scope, set the scope parent,
 * incr nesting level, empty list of entries *)
let openScope () =
  let sco = {
    sco_parent = Some !currentScope;
    sco_nesting = !currentScope.sco_nesting + 1;
    sco_entries = [];
    sco_negofs = start_negative_offset
  } in
  currentScope := sco

(* Close the curr scope, remove from HashTable
 * all scope entries, find scope father and
 * set as current scope *)
let closeScope () =
  let sco = !currentScope in
  let manyentry e = H.remove !tab e.entry_id in
  (* apply manyentry to all sco_entries *)
  List.iter manyentry sco.sco_entries;
  match sco.sco_parent with
  | Some scp ->
      currentScope := scp
  | None ->
      internal "cannot close the outer scope!"

exception Failure_NewEntry of entry

(* Adds a newEntry in the HashTable.
 * and returns it. inf is entry_info *)
let newEntry id inf err =
  try
    if err then begin
      try
        let e = H.find !tab id in
        (* if there is a same id in the current scope, error *)
        if e.entry_scope.sco_nesting = !currentScope.sco_nesting then
           raise (Failure_NewEntry e)
      with Not_found ->
        (* it's ok *)
        ()
    end;
    (* make the new entry *)
    let e = {
      entry_id = id;
      entry_scope = !currentScope;
      entry_info = inf
    } in
    (* add it to HashTable *)
    H.add !tab id e;
    (* add it to current scope entry list *)
    !currentScope.sco_entries <- e :: !currentScope.sco_entries;
    (* return it *)
    e
  with Failure_NewEntry e ->
    error "duplicate identifier %a" pretty_id id;
    e

(* Check if name is in HashTable
 * return entry if found, else raise Not_found/Exit *)
let lookupEntry id how err =
  let scc = !currentScope in
  let lookup () =
    match how with
    | LOOKUP_CURRENT_SCOPE ->
        let e = H.find !tab id in
        if e.entry_scope.sco_nesting = scc.sco_nesting then
          e
        else
          raise Not_found
    | LOOKUP_ALL_SCOPES ->
        H.find !tab id in
  if err then
    try
      lookup ()
    with Not_found ->
      error "unknown identifier %a (first occurrence)"
        pretty_id id;
      (* put it in, so we don't see more errors *)
      H.add !tab id (no_entry id);
      raise Exit
  else
    lookup ()

(* Add a new variable, with type typ and name id,
 * decr the neg offset, create info struct and
 * call NewEntry *)
let newVariable id typ err =
  !currentScope.sco_negofs <- !currentScope.sco_negofs - sizeOfType typ;
  let inf = {
    variable_type = typ;
    variable_offset = !currentScope.sco_negofs
  } in
  newEntry id (ENTRY_variable inf) err

(* Add a new function 
 * return an ENTRY_function entry registered in Hashtable
 * whether it's been for a new or a forwarded function *)
let newFunction id err =
  try
    (* if another entry with same id is in current scope *)
    let e = lookupEntry id LOOKUP_CURRENT_SCOPE false in
    match e.entry_info with
    (* and that entry is a forward function definiton *)
    | ENTRY_function inf when inf.function_isForward ->
        (* edit THAT entry *)
        inf.function_isForward <- false;
        inf.function_pstatus <- PARDEF_CHECK;
        inf.function_redeflist <- inf.function_paramlist;
        (* return THAT entry *)
        e
    | _ ->
        if err then
          error "duplicate identifier: %a" pretty_id id;
          raise Exit
  (* else if id not found in current scope *)
  with Not_found ->
    let inf = {
      function_isForward = false;
      function_paramlist = [];
      function_redeflist = [];
      function_result = TYPE_none;
      function_pstatus = PARDEF_DEFINE;
      function_initquad = 0
    } in
    (* register a new entry *)
    newEntry id (ENTRY_function inf) false

(* Add a new function parameter 
 * Should work fine with PARDEF_{DEFINE | CHECK} *)
let newParameter id typ mode f err =
  match f.entry_info with
  (* f.entry_info must be ENTRY_function *)
  | ENTRY_function inf -> begin
      (* match function parameter status with... *)
      match inf.function_pstatus with
      (* while definition *)
      | PARDEF_DEFINE ->
          (* new parameter_info *)
          let inf_p = {
            parameter_type = typ;
            parameter_offset = 0;
            parameter_mode = mode
          } in
          (* register a new entry for the parameter *)
          let e = newEntry id (ENTRY_parameter inf_p) err in
          (* append the entry to the functions entry list *)
          inf.function_paramlist <- e :: inf.function_paramlist;
          e
      (* while checking *)
      | PARDEF_CHECK -> begin
          match inf.function_redeflist with
          | p :: ps -> begin
              (* "pop" the first parameter *)
              inf.function_redeflist <- ps;
              match p.entry_info with
              (* match it with parameter_info *)
              | ENTRY_parameter inf ->
                  (* type matching *)
                  if not (equalType inf.parameter_type typ) then
                    error "Parameter type mismatch in redeclaration \
                           of function %a" pretty_id f.entry_id
                  (* passing mode matching *)
                  else if inf.parameter_mode != mode then
                    error "Parameter passing mode mismatch in redeclaration \
                           of function %a" pretty_id f.entry_id
                  (* name matching *)
                  else if p.entry_id != id then
                    error "Parameter name mismatch in redeclaration \
                           of function %a" pretty_id f.entry_id
                  (* if all ok, add the paramter entry to the Hashtable *)
                  else
                    H.add !tab id p;
                  (* return the parameter entry *)
                  p
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
      (* while full definition *)
      | PARDEF_COMPLETE ->
          internal "Cannot add a parameter to an already defined function";
          raise Exit
    end
  (* should never reach *)
  | _ ->
      internal "Cannot add a parameter to a non-function";
      raise Exit

(* add a new temporary variable
 * make a new name, decr the negative offset
 * create new temporary_info and register a new Entry *)
let newTemporary typ =
  let id = id_make ("$" ^ string_of_int !tempNumber) in
  !currentScope.sco_negofs <- !currentScope.sco_negofs - sizeOfType typ;
  let inf = {
    temporary_type = typ;
    temporary_offset = !currentScope.sco_negofs
  } in
  incr tempNumber;
  newEntry id (ENTRY_temporary inf) false

(* declare a forwarded function *)
let forwardFunction e =
  match e.entry_info with
  | ENTRY_function inf ->
      inf.function_isForward <- true
  | _ ->
      internal "Cannot make a non-function forward"

(* finish function checking *)
let endFunctionHeader e typ =
  match e.entry_info with
  (* e.entry_info must be ENTRY_function *)
  | ENTRY_function inf ->
      begin
        match inf.function_pstatus with
        (* error *)
        | PARDEF_COMPLETE ->
            internal "Cannot end parameters in an already defined function"
        (* while definition *)
        | PARDEF_DEFINE ->
            (* set the return type *)
            inf.function_result <- typ;
            let offset = ref start_positive_offset in
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
        (* while checking *)
        | PARDEF_CHECK ->
            (* check number of parameters *)
            if inf.function_redeflist <> [] then
              error "Fewer parameters than expected in redeclaration \
                     of function %a" pretty_id e.entry_id;
            (* check return type *)
            if not (equalType inf.function_result typ) then
              error "Result type mismatch in redeclaration of function %a"
                    pretty_id e.entry_id;
      end;
      (* set parameter status as complete *)
      inf.function_pstatus <- PARDEF_COMPLETE
  (* should never reach *)
  | _ ->
      internal "Cannot end parameters in a non-function"
