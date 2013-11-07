(* Pazcal Data Types *)

type typ = TYPE_none        (* no type (used for errors)          *)
         | TYPE_int         (* int                                *)
         | TYPE_bool        (* bool                               *)
         | TYPE_char        (* char                               *)
         | TYPE_REAL        (* REAL                               *)
         | TYPE_array of    (* array                              *)
             typ *          (*   element type                     *)
             int            (*   size of array if known else zero *)
         | TYPE_proc

val sizeOfType : typ -> int
val equalArrayType : typ -> typ -> bool
val equalType : typ -> typ -> bool
val string_of_typ : typ -> string
