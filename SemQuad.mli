(* Semantic Quad datatypes and functions *)

(* Quadruples operands datatype *)
type quad_op_t = Q_none                           (* Error Handling *)
               | Q_int of int                     (* Constant Integer *)
               | Q_real of float                  (* Constant Real *)
               | Q_bool of bool                   (* Constant Boolean *)
               | Q_char of char                   (* Constant Character *)
               | Q_string of string               (* Constant String Literal *)
               | Q_entry of Symbol.entry          (* Symbol Table entry i.e. name, temp *)
               | Q_funct_res                      (* Function result: $$ *)
               | Q_deref of Symbol.entry          (* Dereference: [x] *)
               | Q_addr                           (* Address: {x} *)
               | Q_label of int                   (* label *)
               | Q_pass_mode of quad_pass_mode    (* Pass mode: V, R, RET *)
               | Q_dash                           (* Dash : - *)
               | Q_backpatch                      (* Backpatch : * *)

and quad_pass_mode = V | R | RET

(* Semantic Value of expr *)
type semv_expr = {
  e_place : quad_op_t;
  e_typ : Types.typ     (* Maybe not needed, since place can tell *)
}

val esv_err : semv_expr

(* Semantic Value of cond *)
type semv_cond = {
  c_true : int list;
  c_false : int list
}

val csv_err : semv_cond

(* Semantic Value of stmt *)
type semv_stmt = {
  s_next : int list;
  (* s_code : quad_t list *)
  s_len : int
}

val ssv_empty : semv_stmt

val quad_of_passmode : Symbol.pass_mode -> quad_pass_mode

val string_of_quad_op : quad_op_t -> string

(* Quadruples datatype *)
type quad_t = Q_empty
            | Q_unit of quad_op_t
            | Q_endu of quad_op_t
            | Q_op of string * quad_op_t * quad_op_t * quad_op_t
            | Q_assign of quad_op_t * quad_op_t
            | Q_array of quad_op_t * quad_op_t * quad_op_t
            | Q_relop of string * quad_op_t * quad_op_t * quad_op_t
            | Q_ifb of quad_op_t * quad_op_t
            | Q_jump of quad_op_t
            | Q_label of quad_op_t
            | Q_jl of quad_op_t
            | Q_call of quad_op_t
            | Q_par of quad_op_t * quad_op_t
            | Q_ret

val concat4 : string -> quad_op_t -> quad_op_t -> quad_op_t -> string

val string_of_quad : quad_t -> string

val addNewQuad : quad_t -> unit

val rmLastQuad : unit -> unit

val printIntermediateCode : unit -> unit

val backpatch : int list -> quad_op_t -> unit

val exprQuadLen : int ref

val resetExprQuadLen : unit -> unit

val incExprQuadLen : unit  -> unit

val lvalQuadLen : int ref

val resetLvalQuadLen : unit -> unit

val incLvalQuadLen : unit -> unit

val const_of_quad : quad_op_t -> Symbol.const_val

val quad_of_const : Symbol.const_val -> quad_op_t

val get_binop_pos : unit -> Lexing.position * Lexing.position

val binop_error : Types.typ -> string -> Types.typ -> Lexing.position -> Lexing.position -> unit

val unop_error : string -> Types.typ -> Lexing.position -> Lexing.position -> unit

(* val cond_of_expr : semv_expr -> semv_cond *)
val cond_of_expr : semv_expr -> semv_cond * int

val expr_of_cond : semv_cond -> semv_expr

val sq_binop : semv_expr -> string -> semv_expr -> Lexing.position -> Lexing.position -> semv_expr

val sq_relop : semv_expr -> string -> semv_expr -> Lexing.position -> Lexing.position -> semv_expr

val sq_unop : string -> semv_expr -> Lexing.position -> Lexing.position -> semv_expr

val sq_cdef : string -> Types.typ -> semv_expr -> unit

val sq_lvalue : string -> semv_expr list -> semv_expr

val sq_assign : semv_expr -> string -> semv_expr -> int

val sq_vardef : Types.typ -> string * quad_op_t list * semv_expr -> unit

val sq_rout_head : string -> Types.typ -> (Types.typ * (string * Symbol.pass_mode * quad_op_t list)) list -> Symbol.entry

val sq_rout_call : string -> semv_expr list -> semv_expr

val sq_plus_plus : semv_expr -> quad_t list

val sq_minus_minus : semv_expr -> quad_t list
