type typ = TYPE_none
         | TYPE_int
         | TYPE_bool
         | TYPE_char
         | TYPE_REAL
         | TYPE_array of
             typ *
             int

let rec sizeOfType t =
   match t with
   | TYPE_int            -> 4 (* 32-bit *)
   | TYPE_bool           -> 1
   | TYPE_char           -> 1
   | TYPE_REAL           -> 10 (* IEEE 754 repr. *)
   | TYPE_array (et, sz) -> sz * sizeOfType et
   | _                   -> 0

let equalReal r1 r2 =
  (* TODO : Real comparison ? *)
  printf "Unsupported yet\n";
  false

let rec equalType t1 t2 =
   match t1, t2 with
   | TYPE_array (et1, sz1), TYPE_array (et2, sz2) -> equalType et1 et2
   | TYPE_REAL r1, TYPE_REAL r2                   -> equalReal r1 r2
   | _                                            -> t1 = t2
