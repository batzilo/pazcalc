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

(* convert a string to id *)
let id_make = Hashcons.register_hcons Hid.f ()

(* convert an id to a string *)
let id_name = Hashcons.hash_value

(* print an id *)
let pretty_id ppf id =
  Format.fprintf ppf "%s" (id_name id)
