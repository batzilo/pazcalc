(* Semantic Quad datatypes and functions *)



(* Datatypes *)

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
               | Q_lbl of int                     (* label *)
               | Q_pass_mode of quad_pass_mode    (* Pass mode: V, R, RET *)
               | Q_dash                           (* Dash: -- *)
               | Q_backpatch                      (* Backpatch: * *)

and quad_pass_mode = V | R | RET

(* Semantic Value of expr *)
type semv_expr = {
  e_place : quad_op_t;
  e_typ : Types.typ     (* Maybe not needed, since place can tell *)
}

(* Used for errors *)
val esv_err : semv_expr

(* Semantic Value of cond *)
type semv_cond = {
  c_true : int list;
  c_false : int list
}

(* Used for errors *)
val csv_err : semv_cond

(* Semantic Value of stmt *)
type semv_stmt = {
  s_next : int list;
  (* s_code : quad_t list *)
  s_len : int
}

(* Used for simple cases *)
val ssv_empty : semv_stmt

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
            | Q_jumpl of quad_op_t
            | Q_call of quad_op_t
            | Q_par of quad_op_t * quad_op_t
            | Q_ret



(* Conversions *)

val quad_of_passmode : Symbol.pass_mode -> quad_pass_mode

val const_of_quad : quad_op_t -> Symbol.const_val

val quad_of_const : Symbol.const_val -> quad_op_t

val string_of_quad_op : quad_op_t -> string

val concat4 : string -> quad_op_t -> quad_op_t -> quad_op_t -> string

val string_of_quad : quad_t -> string



(* Intermediate Code Representation *)

val icode : (int * quad_t) list ref

val addNewQuad : quad_t -> unit

val rmLastQuad : unit -> unit

val rmQuad : int list ref -> int -> unit

val printIntermediateCode : unit -> unit

val backpatch : int list -> quad_op_t -> unit



(* Quads produced by expressions *)

val exprQuadLen : int ref

val resetExprQuadLen : unit -> unit

val incExprQuadLen : unit  -> unit



(* Quads produced by lvalue *)

val lvalQuadLen : int ref

val resetLvalQuadLen : unit -> unit

val incLvalQuadLen : unit -> unit

(*
val get_binop_pos : unit -> Lexing.position * Lexing.position
*)



(* Error Handling *)

val unop_error : string -> Types.typ -> Lexing.position -> Lexing.position -> unit

val binop_error : Types.typ -> string -> Types.typ -> Lexing.position -> Lexing.position -> unit



(* A simple optimization *)

val const_unop : string * Types.typ * Symbol.const_val -> quad_op_t

val const_binop : string * Types.typ * Symbol.const_val * Symbol.const_val -> quad_op_t



(* Semantic Actions and Icode generation *)

val sq_cdef : string -> Types.typ -> semv_expr -> unit

val what_un_type : string -> semv_expr -> Types.typ

val sq_unop : string -> semv_expr -> Lexing.position -> Lexing.position -> semv_expr

val what_bin_type : semv_expr -> string -> semv_expr -> Types.typ

val sq_binop : semv_expr -> string -> semv_expr -> Lexing.position -> Lexing.position -> semv_expr

val cond_of_expr : semv_expr -> semv_cond

val expr_of_cond : semv_cond -> semv_expr

val sq_relop : semv_expr -> string -> semv_expr -> Lexing.position -> Lexing.position -> semv_expr

val sq_lvalue : string -> semv_expr list -> semv_expr

val sq_assign : semv_expr -> string -> semv_expr -> unit

val sq_vardef : Types.typ -> string * quad_op_t list * semv_expr -> unit

val sq_rout_head : string -> Types.typ -> (Types.typ * (string * Symbol.pass_mode * quad_op_t list)) list -> Symbol.entry

val sq_rout_call : string -> semv_expr list -> semv_expr

val sq_plus_plus : semv_expr -> unit

val sq_minus_minus : semv_expr -> unit



(* Break *)

val breakQuad : int list ref

val addBreakQuad : int list -> unit

val resetBreakQuad : unit -> unit

val collectMyBreaks : int -> int -> unit



(* Continue *)

val contQuad : int list ref

val addContQuad : int list -> unit

val resetContQuad : unit -> unit

val collectMyConts : int -> int -> unit



(* FOR loop *)

val sq_range : semv_expr -> semv_expr -> semv_expr -> quad_op_t * quad_op_t * quad_op_t

val sq_for_control : string -> quad_op_t -> quad_op_t -> quad_op_t -> int * semv_cond * int * quad_t list

val sq_format : semv_expr * semv_expr * semv_expr -> semv_expr * semv_expr * semv_expr




(* Statements *)

val st_block : semv_stmt -> semv_stmt -> semv_stmt

val st_constdef : unit -> semv_stmt

val st_vardef : unit -> semv_stmt

val st_cond_of_expr : semv_expr -> semv_cond * int

val st_fly : unit -> int list * int

val st_assign : semv_expr -> string -> semv_expr -> semv_stmt

val st_plusplus : semv_expr -> semv_stmt

val st_minusminus : semv_expr -> semv_stmt

val st_if_then : semv_cond * int -> semv_stmt -> semv_stmt

val st_if_then_else : semv_cond * int -> semv_stmt -> int list * int -> semv_stmt -> semv_stmt

val st_while : semv_cond * int -> semv_stmt -> semv_stmt

val st_for : int * semv_cond * int * quad_t list -> semv_stmt -> semv_stmt

val st_do_while : semv_stmt -> semv_cond * int -> semv_stmt

val st_break : unit -> semv_stmt

val st_continue : unit -> semv_stmt

val st_return_simple : unit -> semv_stmt

val st_return : semv_expr -> semv_stmt
