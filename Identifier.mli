(* Hash consed identifiers *)

(* type made abstract *)
type id

(* convert a string to id *)
val id_make  : string -> id

(* convert an id to a string *)
val id_name  : id -> string

(* print an id *)
val pretty_id : Format.formatter -> id -> unit
