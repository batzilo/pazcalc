(* Polymorphic hash consing,
   you don't need to understand what this is all about... *)

(* type made abstract *)
type 'a hash_consed

(* some functions on the abstract type *)
val hash_value : 'a hash_consed -> 'a
val hash_tag : 'a hash_consed -> int

(* Comp is a named module type (=signature) *)
module type Comp =
  sig
    type t
    val equal : t -> t -> bool
    val hash : t -> int
  end

(* S is a named module type (=signature) *)
module type S =
  sig
    type t
    val f : unit -> t -> t hash_consed
  end

(* Functor 
 * with one argument : X that is a Comp module
 * returns a module that is an S module
 *)
module Make (X : Comp) : (S with type t = X.t)

val init : unit -> unit
val register_hcons : (unit -> 'a -> 'a hash_consed) ->
                     (unit -> 'a -> 'a hash_consed)
