(* Symbol table functions *)

(* parameter pass mode *)
type pass_mode = PASS_BY_VALUE | PASS_BY_REFERENCE

(* parameter status *)
type param_status =
  | PARDEF_COMPLETE                           (* Πλήρης ορισμός     *)
  | PARDEF_DEFINE                             (* Εν μέσω ορισμού    *)
  | PARDEF_CHECK                              (* Εν μέσω ελέγχου    *)

(* scope datatype *)
type scope = {
  sco_parent : scope option;
  sco_nesting : int;
  mutable sco_entries : entry list;
  mutable sco_negofs : int
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

(* HashTable entry information datatype *)
and entry_info = ENTRY_none
               | ENTRY_variable of variable_info
               | ENTRY_function of function_info
               | ENTRY_parameter of parameter_info
               | ENTRY_temporary of temporary_info

(* HashTable entry datatype *)
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

(* Open a new scope, set the scope parent,
 * incr nesting level, empty list of entries *)
val openScope        : unit -> unit

(* Close the curr scope, remove from HashTable
 * all scope entries, find scope father and
 * set as current scope *)
val closeScope       : unit -> unit

(* Check if name is in HashTable
 * return entry if found, else raise Not_found/Exit *)
val lookupEntry       : Identifier.id -> lookup_type -> bool -> entry

(* Add a new variable, with type typ and name id,
 * decr the neg offset, create info struct and
 * call NewEntry *)
val newVariable      : Identifier.id -> Types.typ -> bool -> entry

(* Add a new function 
 * return an ENTRY_function entry registered in Hashtable
 * whether it's been for a new or a forwarded function *)
val newFunction      : Identifier.id -> bool -> entry

(* Add a new function parameter 
 * Should work fine with PARDEF_{DEFINE | CHECK} *)
val newParameter     : Identifier.id -> Types.typ -> pass_mode ->
                                        entry -> bool -> entry

(* add a new temporary variable
 * make a new name, decr the negative offset
 * create new temporary_info and register a new Entry *)
val newTemporary     : Types.typ -> entry

(* declare a forwarded function *)
val forwardFunction   : entry -> unit

(* finish function checking *)
val endFunctionHeader : entry -> Types.typ -> unit
