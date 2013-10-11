(* Pazcal Data Types *)

type typ = TYPE_none        (* no type (should not be used)       *)
         | TYPE_int         (* int                                *)
         | TYPE_bool        (* bool                               *)
         | TYPE_char        (* char                               *)
         | TYPE_REAL        (* REAL                               *)
         | TYPE_array of    (* array                              *)
             typ *          (*   element type                     *)
             int            (*   size of array, if known, or zero *)
         (*
         | TYPE_proc        (* proc (return type)                 *)
         | TODO : unfinished types???
         *)

val sizeOfType : typ -> int
val equalReal : typ -> typ -> bool
val equalType : typ -> typ -> bool
