(* id is 'a hash_consed *)
type id = string Hashcons.hash_consed

(* Functor application
 * struct is module of type Comp
 * Hid is module of type S
 *)
module Hid = Hashcons.Make (
  struct
    type t = string
    let equal = (=)
    let hash = Hashtbl.hash
  end
)

(* register a new id *)
let id_make = Hashcons.register_hcons Hid.f ()

(* get the name's hash? *)
let id_name = Hashcons.hash_value

(* pretty print the id *)
let pretty_id ppf id =
  Format.fprintf ppf "%s" (id_name id)
