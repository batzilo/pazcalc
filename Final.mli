
val header : unit -> string

val footer : unit -> string

val transform : int * SemQuad.quad_t -> string

val generate : (int * SemQuad.quad_t) list -> string
