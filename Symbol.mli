(* Symbol table functions
 *
 * Symbol table stores pairs of the form ( id, entry )
 * Use Identifier.id_make to convert a string to an id
 *
 *)

val debug : bool

(* type definition for parameter passing *)
type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE

(* type definition for parameter status *)
type param_status =
  | PARDEF_COMPLETE         (* Πλήρης ορισμός  *)
  | PARDEF_DEFINE           (* Εν μέσω ορισμού *)
  | PARDEF_CHECK            (* Εν μέσω ελέγχου *)

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
  mutable function_initquad  : int            (* Αρχική τετράδα        *)
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

val start_positive_offset : int           (* Αρχικό θετικό offset στο Ε.Δ.   *)
val start_negative_offset : int           (* Αρχικό αρνητικό offset στο Ε.Δ. *)

val currentScope : scope ref              (* Τρέχουσα εμβέλεια         *)
val quadNext : int ref                    (* Αριθμός επόμενης τετράδας *)
val tempNumber : int ref                  (* Αρίθμηση των temporaries  *)

(* Initialize the Symbol Table *)
val initSymbolTable  : int -> unit

(* Add a new scope with an empty list of entries and
 * increased nesting level and set it as current scope *)
val openScope        : unit -> unit

(* Close the current scope, remove from HashTable
 * all scope entries, and set scope father as current scope *)
val closeScope       : unit -> unit

(* Check if entry exists in Symbol Table
 * return entry if found, else raise Not_found/Exit
 * err = true handles not_found with an error message
 * err = false does nothing (used for function forward definitions) *)
val lookupEntry      : Identifier.id -> lookup_type -> bool -> entry

(* Add a new variable to the Symbol Table
 * name is id (after id_make), type is typ 
 * err = true means search the ST for possible duplicate
 * call NewEntry *)
val newVariable      : Identifier.id -> Types.typ -> bool -> entry

(* Add a new constant,
 * name is id (after id_make), type is typ and value is v
 * err = true means search the ST for possible duplicate
 * call NewEntry *)
val newConstant      : Identifier.id -> Types.typ -> const_val -> bool -> entry

(* Make a new temporary variable
 * call newEntry *)
val newTemporary     : Types.typ -> entry

(* Add a new function to the Symbol Table
 * err should be true
 * if function not exists in ST then call newEntry
 * else if function was forwarded, start parameter checking *)
val newFunction      : Identifier.id -> bool -> entry

(* Add a new function parameter
 * err should be true
 * if function par status is PARDEF_DEFINE, add par to ST
 * if function par status is PARDEF_CHECK, check for compatibility
 * with forward definition and add par to ST
 * call newEntry *)
val newParameter     : Identifier.id -> Types.typ -> pass_mode ->
                                        entry -> bool -> entry

(* set a function to be forwarded *)
val forwardFunction   : entry -> unit

(* finish function checking
 * typ is function return type
 * after function definition or after function check *)
val endFunctionHeader : entry -> Types.typ -> unit
