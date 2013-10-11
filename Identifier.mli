(* Hash consed identifiers *)

(* type made abstract *)
type id

(* register a new id *)
val id_make  : string -> id

(* get the name's hash? *)
val id_name  : id -> string

(* pretty print the id *)
val pretty_id : Format.formatter -> id -> unit
