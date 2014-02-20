
val header : string -> string

val footer : unit -> string

val transform : int * SemQuad.quad_t -> string

val generate : string -> (int * SemQuad.quad_t) list -> string
