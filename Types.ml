(* Pazcal Data Types *)

type typ = TYPE_none        (* no type (used for errors)            *)
         | TYPE_int         (* int                                  *)
         | TYPE_bool        (* bool                                 *)
         | TYPE_char        (* char                                 *)
         | TYPE_REAL        (* REAL                                 *)
         | TYPE_array of    (* array                                *)
             typ *          (*   element type                       *)
             int            (*   size of array if known else zero   *)
         | TYPE_proc        (* Procedure type                       *)

let rec sizeOfType t =
  (* in bytes *)
  match t with
  | TYPE_int            -> 2    (* 16-bit *)
  | TYPE_bool           -> 1
  | TYPE_char           -> 1
  | TYPE_REAL           -> 10   (* IEEE 754 repr. *)
  | TYPE_array (et, sz) -> sz * sizeOfType et
  | _                   -> 0

let rec equalType t1 t2 =
   match t1, t2 with
   | TYPE_array (et1, sz1), TYPE_array (et2, sz2) -> equalType et1 et2
   | _                                            -> t1 = t2

let rec equalArrayType t1 t2 =
   match t1, t2 with
   | TYPE_array (et1, sz1), TYPE_array (et2, sz2) -> if sz1 = sz2 then equalArrayType et1 et2 else false
   | _ -> t1 = t2

(* convert Type to string *)
let rec string_of_typ typ =
  match typ with
  | TYPE_none -> "<undefined>"
  | TYPE_int -> "int"
  | TYPE_bool -> "bool"
  | TYPE_char -> "char"
  | TYPE_REAL -> "REAL"
  | TYPE_array(et,sz) when sz > 0 -> String.concat "" [(string_of_typ et);("[");(string_of_int sz);("]")]
  | TYPE_array(et,sz) -> String.concat "" [(string_of_typ et);("[]")]
  | TYPE_proc -> "PROC"
