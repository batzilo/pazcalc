(* Quadruples *)

open Printf
open Lexing
open Parsing

open Error
open Identifier
open Symbol
open Types

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

(* Used for errors *)
let esv_err = {
  e_place = Q_none;
  e_typ = TYPE_none;
}

(* Semantic Value of cond *)
type semv_cond = {
  c_true : int list;
  c_false : int list
}

(* Used for errors *)
let csv_err = {
  c_true = [];
  c_false = []
}

(* Semantic Value of stmt *)
type semv_stmt = {
  s_next : int list;
  (* s_code : quad_t list *)
  s_len : int
}

let ssv_empty = {
  s_next = [];
  (* s_code = [] *)
  s_len = 0
}

(* convert Symbol.pass_mode to SemQuad.quad_pass_mode *)
let quad_of_passmode = function
  | PASS_BY_VALUE -> V
  | PASS_BY_REFERENCE -> R

(* convert SemQuad.quad_op_t to string *)
let string_of_quad_op = function
  | Q_none -> ""
  | Q_int i -> string_of_int i
  | Q_real r -> string_of_float r
  | Q_bool b -> string_of_bool b
  | Q_char c -> Char.escaped c
  | Q_string s -> s
  | Q_entry e -> id_name e.entry_id
  | Q_funct_res -> "$$"
  | Q_deref e -> "[" ^ id_name e.entry_id ^ "]"
  | Q_label l -> string_of_int l
  | Q_pass_mode V -> "V"
  | Q_pass_mode R -> "R"
  | Q_pass_mode RET -> "RET"
  | Q_dash -> "--"
  | Q_backpatch -> "*"
  | _ -> "<stub!>"

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

(* concatenate four strings seperated by commas *)
let concat4 a b c d =
  let x = ", " in
  let s y = string_of_quad_op y in
  a ^ x ^ s b ^ x ^ s c ^ x ^ s d

(* convert SemQuad.quad_t to string *)
let string_of_quad = function
  | Q_empty ->              ""
  | Q_unit u ->             concat4 "unit"  u       Q_dash  Q_dash
  | Q_endu u ->             concat4 "endu"  u       Q_dash  Q_dash
  | Q_op (op,x,y,z) ->      concat4 op      x       y       z
  | Q_assign (x,z) ->       concat4 ":="    x       Q_dash  z
  | Q_array (x,y,z) ->      concat4 "array" x       y       z
  | Q_relop (op,x,y,z) ->   concat4 op      x       y       z
  | Q_ifb (x,z) ->          concat4 "ifb"   x       Q_dash  z
  | Q_jump z ->             concat4 "jump"  Q_dash  Q_dash  z
  | Q_label l ->            concat4 "label" l       Q_dash  Q_dash
  | Q_jl l ->               concat4 "jumpl" Q_dash  Q_dash  l
  | Q_call u ->             concat4 "call"  Q_dash  Q_dash  u
  | Q_par (x,m) ->          concat4 "par"   x       m       Q_dash
  | Q_ret ->                concat4 "ret"   Q_dash  Q_dash  Q_dash

(* intermediate code : list of quads *)
let icode = ref []

(* a function to add a new quad to intermediate code *)
let addNewQuad quad =
  let q = (!quadNext, quad) in
  !icode <- q :: !icode;
  !quadNext <- !quadNext + 1;
  ()

(* remove last quad from intermediate code *)
let rmLastQuad () =
  match !icode with
  | h::t ->
    !icode <- t;
    !quadNext <- !quadNext - 1;
    ()
  | _ ->
    error "intermediate code is empty!";
    raise Exit

(* print every quad in intermediate code quad list *)
let printIntermediateCode () =
  let pr (n,quad) =
    printf "%3d: %s\n" n (string_of_quad quad)
  in
    printf "\n\nIntermediate code:\n";
    List.iter pr !icode;
    printf "\n"

(* backpatch *)
let backpatch l nz =
  (* make a new quad out of the old one *)
  let fix quad =
    match quad with
    | Q_relop (op,x,y,z) -> Q_relop (op,x,y,nz)
    | Q_jump z -> Q_jump nz
    | Q_ifb (x,z) -> Q_ifb (x,nz)
    | _ -> printf "error\n"; printf "%s\n" (string_of_quad quad); internal "cannot backpatch that quad!"; raise Exit
  in
  (* check if n is in l *)
  let is_to_be_fixed n =
    let rec find lst =
      match lst with
      | h::t -> if ( h = n ) then true else find t
      | _ -> false
    in find l
  in
  (* go through every quad and produce a new quad list with quads backpatched *)
  let rec walk ic =
    match ic with
    | (hn,hq)::t -> if ( is_to_be_fixed hn ) then (hn,(fix hq))::(walk t) else (hn,hq)::(walk t)
    | [] -> []
  in
  let newicode = walk !icode in
  (* replace icode with backpatced icode *)
  !icode <- newicode

let exprQuadLen = ref 0

let resetExprQuadLen () =
  !exprQuadLen <- 0

let incExprQuadLen () =
  !exprQuadLen <- !exprQuadLen + 1

let lvalQuadLen = ref 0

let resetLvalQuadLen () =
  !lvalQuadLen <- 0

let incLvalQuadLen () =
  !lvalQuadLen <- !lvalQuadLen + 1

(* convert SemQuad.quad_op_t to Symbol.const_val *)
let const_of_quad = function
    | Q_int q -> CONST_int q
    | Q_real q -> CONST_REAL q
    | Q_bool q -> CONST_bool q
    | Q_char q -> CONST_char q
    | _ -> CONST_none

(* convert Symbol.const_val to SemQuad.quad_op_t *)
let quad_of_const = function
    | CONST_int c -> Q_int c
    | CONST_REAL c -> Q_real c
    | CONST_bool c -> Q_bool c 
    | CONST_char c -> Q_char c
    | _ -> Q_none


(* Simple Function to get Expression Position *)
let get_binop_pos () =
    (rhs_start_pos 1, rhs_start_pos 3)

(*
(* print an error message *)
let print_binop_type_error op_name t1 t2 exp_t sp ep =
    error
    "Type Mismatch: Operator (%s) and operand don't agree\n\
    \tOperator Domain:\t%s * %s\n\
    \tOperand:\t\t%s * %s\n\
    \tIn expression starting at line %d position %d, ending\
    at line %d position %d."
    (op_name)
    (string_of_typ exp_t) (string_of_typ exp_t)
    (string_of_typ t1) (string_of_typ t2)
    (sp.pos_lnum) (sp.pos_cnum - sp.pos_bol)
    (ep.pos_lnum) (ep.pos_cnum - ep.pos_bol)
*)

(* Print an error message *)
let binop_error a op b x y =
  error
  "Binary Operator \"%s\" Error : \
  Line %d Position %d through Line %d Position %d\n\
  Operands Type Mismatch : \
  Can't apply \"%s\" to \"%s\" and \"%s\"\n"
  op
  x.pos_lnum (x.pos_cnum - x.pos_bol)
  y.pos_lnum (y.pos_cnum - y.pos_bol)
  op (string_of_typ a) (string_of_typ b)

(*
(* print an error message *)
let print_unary_type_error op_name t pos =
    error 
    "Type Mismatch: Unary Operator (%s) and operand don't agree\n\
    \tOperator Domain:\t int\
    \tOperand Domain: \t %s\
    \tIn expression at line %d, position %d."
    (op_name)
    (string_of_typ t)
    (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol)
*)

(* Print an error message *)
let unop_error op a x y =
  error
  "Unary Operator \"%s\" Error : \
  Line %d Position %d through Line %d Position %d\n\
  Operand Type Mismatch : \
  Can't apply \"%s\" to \"%s\"\n"
  op
  x.pos_lnum (x.pos_cnum - x.pos_bol)
  y.pos_lnum (y.pos_cnum - y.pos_bol)
  op (string_of_typ a)

let cond_of_expr e =
  let tr = [!quadNext] in
  let q1 = Q_ifb (e.e_place, Q_backpatch) in
  addNewQuad q1;
  let fa = [!quadNext] in
  let q2 = Q_jump (Q_backpatch) in
  addNewQuad q2;
  let c = {
    c_true = tr;
    c_false = fa
  } in (c,2)

let expr_of_cond c =
  let e = newTemporary TYPE_bool in
  backpatch c.c_true (Q_int !quadNext);
  let q = Q_assign ( Q_bool true, Q_entry e) in
  addNewQuad q;
  incExprQuadLen ();
  let foo = !quadNext + 2 in
  let q = Q_jump ( Q_int foo ) in
  addNewQuad q;
  incExprQuadLen ();
  backpatch c.c_false (Q_int !quadNext);
  let q = Q_assign ( Q_bool false, Q_entry e) in
  addNewQuad q;
  incExprQuadLen ();
  let esv = {
    e_place = Q_entry e;
    e_typ = TYPE_bool
  } in esv

(* DEPRECATED *)
(*
type sem_quad_t = {
  place : quad_op_t;
  typ : Types.typ;
  mutable q_next : int list;
  mutable q_true : int list;
  mutable q_false : int list
}
*)

(* DEPRECATED *)
(* Semantic Value of parameters *)
(*
type semv_par = {
  p_name : Identifier.id;
  p_typ : Types.typ;
  p_mode : Symbol.pass_mode
}
*)

(* Semantically check binary expression operand types
 * and find return type according to compatibility *)
let what_bin_type a op b =
    match op with
    | "+"
    | "-"
    | "*"
    | "/" ->
      begin
      match (a.e_typ, b.e_typ) with
      | (TYPE_int, TYPE_int)
      | (TYPE_int, TYPE_char)
      | (TYPE_char, TYPE_int)
      | (TYPE_char, TYPE_char) ->
        TYPE_int
      | (TYPE_int, TYPE_REAL)
      | (TYPE_char, TYPE_REAL)
      | (TYPE_REAL, TYPE_int)
      | (TYPE_REAL, TYPE_char)
      | (TYPE_REAL, TYPE_REAL) ->
        TYPE_REAL
      | (_,_) ->
        TYPE_none
      end
    | "%" ->
      begin
      match (a.e_typ, b.e_typ) with
      | (TYPE_int, TYPE_int)
      | (TYPE_int, TYPE_char)
      | (TYPE_char, TYPE_int)
      | (TYPE_char, TYPE_char) ->
        TYPE_int
      | (_,_) ->
        TYPE_none
      end
    | "=="
    | "!="
    | "<"
    | ">"
    | "<="
    | ">=" ->
      begin
      match (a.e_typ, b.e_typ) with
      | (TYPE_int, TYPE_int)
      | (TYPE_int, TYPE_char)
      | (TYPE_char, TYPE_int)
      | (TYPE_char, TYPE_char)
      | (TYPE_int, TYPE_REAL)
      | (TYPE_char, TYPE_REAL)
      | (TYPE_REAL, TYPE_int)
      | (TYPE_REAL, TYPE_char)
      | (TYPE_REAL, TYPE_REAL) ->
        TYPE_bool
      | (_,_) ->
        TYPE_none
      end
    | "&&"
    | "||" ->
      begin
      match (a.e_typ, b.e_typ) with
      | (TYPE_bool, TYPE_bool) ->
        TYPE_bool
      | (_,_) ->
        TYPE_none
      end
    | _ ->
      TYPE_none

(* Semantically check unary expression operand type and find return type *)
let what_un_type op a =
    match op with
    | "+"
    | "-" ->
      begin
      match a.e_typ with
      | TYPE_int -> TYPE_int
      | TYPE_char -> TYPE_char
      | TYPE_REAL -> TYPE_REAL
      | _ -> TYPE_none
      end
    | "!" ->
      begin
      match a.e_typ with
      | TYPE_bool -> TYPE_bool
      | _ -> TYPE_none
      end
    | _ ->
      TYPE_none

(* Apply a binary operator to constant operands *)
let const_binop = function
    (* int add *)
    | "+", TYPE_int, CONST_int x, CONST_int y -> Q_int ( x + y )
    | "+", TYPE_int, CONST_int x, CONST_char y -> Q_int ( x + int_of_char y )
    | "+", TYPE_int, CONST_char x, CONST_int y -> Q_int ( int_of_char x + y )
    | "+", TYPE_int, CONST_char x, CONST_char y -> Q_int ( int_of_char x + int_of_char y )
    (* REAL add *)
    | "+", TYPE_REAL, CONST_int x, CONST_REAL y -> Q_real ( (float x) +. y )
    | "+", TYPE_REAL, CONST_char x, CONST_REAL y -> Q_real ( (float (int_of_char x)) +. y )
    | "+", TYPE_REAL, CONST_REAL x, CONST_int y -> Q_real ( x +. (float y) )
    | "+", TYPE_REAL, CONST_REAL x, CONST_char y -> Q_real ( x +. (float (int_of_char y)) )
    | "+", TYPE_REAL, CONST_REAL x, CONST_REAL y -> Q_real ( x +. y )
    (* int sub *)
    | "-", TYPE_int, CONST_int x, CONST_int y -> Q_int ( x - y )
    | "-", TYPE_int, CONST_int x, CONST_char y -> Q_int ( x - int_of_char y )
    | "-", TYPE_int, CONST_char x, CONST_int y -> Q_int ( int_of_char x - y )
    | "-", TYPE_int, CONST_char x, CONST_char y -> Q_int ( int_of_char x - int_of_char y )
    (* REAL sub *)
    | "-", TYPE_REAL, CONST_int x, CONST_REAL y -> Q_real ( (float x) -. y )
    | "-", TYPE_REAL, CONST_char x, CONST_REAL y -> Q_real ( (float (int_of_char x)) -. y )
    | "-", TYPE_REAL, CONST_REAL x, CONST_int y -> Q_real ( x -. (float y) )
    | "-", TYPE_REAL, CONST_REAL x, CONST_char y -> Q_real ( x -. (float (int_of_char y)) )
    | "-", TYPE_REAL, CONST_REAL x, CONST_REAL y -> Q_real ( x -. y )
    (* int mul *)
    | "*", TYPE_int, CONST_int x, CONST_int y -> Q_int ( x * y )
    | "*", TYPE_int, CONST_int x, CONST_char y -> Q_int ( x * int_of_char y )
    | "*", TYPE_int, CONST_char x, CONST_int y -> Q_int ( int_of_char x * y )
    | "*", TYPE_int, CONST_char x, CONST_char y -> Q_int ( int_of_char x * int_of_char y )
    (* REAL mul *)
    | "*", TYPE_REAL, CONST_int x, CONST_REAL y -> Q_real ( (float x) *. y )
    | "*", TYPE_REAL, CONST_char x, CONST_REAL y -> Q_real ( (float (int_of_char x)) *. y )
    | "*", TYPE_REAL, CONST_REAL x, CONST_int y -> Q_real ( x *. (float y) )
    | "*", TYPE_REAL, CONST_REAL x, CONST_char y -> Q_real ( x *. (float (int_of_char y)) )
    | "*", TYPE_REAL, CONST_REAL x, CONST_REAL y -> Q_real ( x *. y )
    (* int div *)
    | "/", TYPE_int, CONST_int x, CONST_int y -> Q_int ( x / y )
    | "/", TYPE_int, CONST_int x, CONST_char y -> Q_int ( x / int_of_char y )
    | "/", TYPE_int, CONST_char x, CONST_int y -> Q_int ( int_of_char x / y )
    | "/", TYPE_int, CONST_char x, CONST_char y -> Q_int ( int_of_char x / int_of_char y )
    (* REAL div *)
    | "/", TYPE_REAL, CONST_int x, CONST_REAL y -> Q_real ( (float x) /. y )
    | "/", TYPE_REAL, CONST_char x, CONST_REAL y -> Q_real ( (float (int_of_char x)) /. y )
    | "/", TYPE_REAL, CONST_REAL x, CONST_int y -> Q_real ( x /. (float y) )
    | "/", TYPE_REAL, CONST_REAL x, CONST_char y -> Q_real ( x /. (float (int_of_char y)) )
    | "/", TYPE_REAL, CONST_REAL x, CONST_REAL y -> Q_real ( x /. y )
    (* int mod *)
    | "%", TYPE_int, CONST_int x, CONST_int y -> Q_int ( x mod y )
    | "%", TYPE_int, CONST_int x, CONST_char y -> Q_int ( x mod int_of_char y )
    | "%", TYPE_int, CONST_char x, CONST_int y -> Q_int ( int_of_char x mod y )
    | "%", TYPE_int, CONST_char x, CONST_char y -> Q_int ( int_of_char x mod int_of_char y )
    (* == *)
    | "==", TYPE_bool, CONST_int x, CONST_int y -> Q_bool ( x == y )
    | "==", TYPE_bool, CONST_int x, CONST_char y -> Q_bool ( x == int_of_char y )
    | "==", TYPE_bool, CONST_char x, CONST_int y -> Q_bool ( int_of_char x == y )
    | "==", TYPE_bool, CONST_char x, CONST_char y -> Q_bool ( int_of_char x == int_of_char y )
    | "==", TYPE_bool, CONST_int x, CONST_REAL y -> Q_bool ( (float x) == y )
    | "==", TYPE_bool, CONST_char x, CONST_REAL y -> Q_bool ( (float (int_of_char x)) == y )
    | "==", TYPE_bool, CONST_REAL x, CONST_int y -> Q_bool ( x == (float y) )
    | "==", TYPE_bool, CONST_REAL x, CONST_char y -> Q_bool ( x == (float (int_of_char y)) )
    | "==", TYPE_bool, CONST_REAL x, CONST_REAL y -> Q_bool ( x == y )
    (* != *)
    | "!=", TYPE_bool, CONST_int x, CONST_int y -> Q_bool ( x != y )
    | "!=", TYPE_bool, CONST_int x, CONST_char y -> Q_bool ( x != int_of_char y )
    | "!=", TYPE_bool, CONST_char x, CONST_int y -> Q_bool ( int_of_char x != y )
    | "!=", TYPE_bool, CONST_char x, CONST_char y -> Q_bool ( int_of_char x != int_of_char y )
    | "!=", TYPE_bool, CONST_int x, CONST_REAL y -> Q_bool ( (float x) != y )
    | "!=", TYPE_bool, CONST_char x, CONST_REAL y -> Q_bool ( (float (int_of_char x)) != y )
    | "!=", TYPE_bool, CONST_REAL x, CONST_int y -> Q_bool ( x != (float y) )
    | "!=", TYPE_bool, CONST_REAL x, CONST_char y -> Q_bool ( x != (float (int_of_char y)) )
    | "!=", TYPE_bool, CONST_REAL x, CONST_REAL y -> Q_bool ( x != y )
    (* < *)
    | "<", TYPE_bool, CONST_int x, CONST_int y -> Q_bool ( x < y )
    | "<", TYPE_bool, CONST_int x, CONST_char y -> Q_bool ( x < int_of_char y )
    | "<", TYPE_bool, CONST_char x, CONST_int y -> Q_bool ( int_of_char x < y )
    | "<", TYPE_bool, CONST_char x, CONST_char y -> Q_bool ( int_of_char x < int_of_char y )
    | "<", TYPE_bool, CONST_int x, CONST_REAL y -> Q_bool ( (float x) < y )
    | "<", TYPE_bool, CONST_char x, CONST_REAL y -> Q_bool ( (float (int_of_char x)) < y )
    | "<", TYPE_bool, CONST_REAL x, CONST_int y -> Q_bool ( x < (float y) )
    | "<", TYPE_bool, CONST_REAL x, CONST_char y -> Q_bool ( x < (float (int_of_char y)) )
    | "<", TYPE_bool, CONST_REAL x, CONST_REAL y -> Q_bool ( x < y )
    (* > *)
    | ">", TYPE_bool, CONST_int x, CONST_int y -> Q_bool ( x > y )
    | ">", TYPE_bool, CONST_int x, CONST_char y -> Q_bool ( x > int_of_char y )
    | ">", TYPE_bool, CONST_char x, CONST_int y -> Q_bool ( int_of_char x > y )
    | ">", TYPE_bool, CONST_char x, CONST_char y -> Q_bool ( int_of_char x > int_of_char y )
    | ">", TYPE_bool, CONST_int x, CONST_REAL y -> Q_bool ( (float x) > y )
    | ">", TYPE_bool, CONST_char x, CONST_REAL y -> Q_bool ( (float (int_of_char x)) > y )
    | ">", TYPE_bool, CONST_REAL x, CONST_int y -> Q_bool ( x > (float y) )
    | ">", TYPE_bool, CONST_REAL x, CONST_char y -> Q_bool ( x > (float (int_of_char y)) )
    | ">", TYPE_bool, CONST_REAL x, CONST_REAL y -> Q_bool ( x > y )
    (* <= *)
    | "<=", TYPE_bool, CONST_int x, CONST_int y -> Q_bool ( x <= y )
    | "<=", TYPE_bool, CONST_int x, CONST_char y -> Q_bool ( x <= int_of_char y )
    | "<=", TYPE_bool, CONST_char x, CONST_int y -> Q_bool ( int_of_char x <= y )
    | "<=", TYPE_bool, CONST_char x, CONST_char y -> Q_bool ( int_of_char x <= int_of_char y )
    | "<=", TYPE_bool, CONST_int x, CONST_REAL y -> Q_bool ( (float x) <= y )
    | "<=", TYPE_bool, CONST_char x, CONST_REAL y -> Q_bool ( (float (int_of_char x)) <= y )
    | "<=", TYPE_bool, CONST_REAL x, CONST_int y -> Q_bool ( x <= (float y) )
    | "<=", TYPE_bool, CONST_REAL x, CONST_char y -> Q_bool ( x <= (float (int_of_char y)) )
    | "<=", TYPE_bool, CONST_REAL x, CONST_REAL y -> Q_bool ( x <= y )
    (* >= *)
    | ">=", TYPE_bool, CONST_int x, CONST_int y -> Q_bool ( x >= y )
    | ">=", TYPE_bool, CONST_int x, CONST_char y -> Q_bool ( x >= int_of_char y )
    | ">=", TYPE_bool, CONST_char x, CONST_int y -> Q_bool ( int_of_char x >= y )
    | ">=", TYPE_bool, CONST_char x, CONST_char y -> Q_bool ( int_of_char x >= int_of_char y )
    | ">=", TYPE_bool, CONST_int x, CONST_REAL y -> Q_bool ( (float x) >= y )
    | ">=", TYPE_bool, CONST_char x, CONST_REAL y -> Q_bool ( (float (int_of_char x)) >= y )
    | ">=", TYPE_bool, CONST_REAL x, CONST_int y -> Q_bool ( x >= (float y) )
    | ">=", TYPE_bool, CONST_REAL x, CONST_char y -> Q_bool ( x >= (float (int_of_char y)) )
    | ">=", TYPE_bool, CONST_REAL x, CONST_REAL y -> Q_bool ( x >= y )
    (* && *)
    | "&&", TYPE_bool, CONST_bool x, CONST_bool y -> Q_bool ( x && y )
    (* || *)
    | "||", TYPE_bool, CONST_bool x, CONST_bool y -> Q_bool ( x || y )
    (* anything else *)
    | _ -> Q_none

(* Semantic-Quad actions for binary operators *)
let sq_binop a op b x y =
    (* TODO: division by zero check *)
    let typ = what_bin_type a op b in
    match typ with
    | TYPE_none ->
      (* if TYPE_none, we have an error *)
      binop_error a.e_typ op b.e_typ x y;
      esv_err
    | _ ->
      (* Check if Constants *)
      (* TODO add bool param for this *)
      let c1 = const_of_quad a.e_place in
      let c2 = const_of_quad b.e_place in
      match c1,c2 with
      | CONST_none, _
      | _, CONST_none ->
         (* No constant *)
         let e = newTemporary typ in
         let q = Q_op (op, a.e_place, b.e_place, Q_entry e) in
         let esv = {
           e_place = (Q_entry e);
           e_typ = typ
         } in
         addNewQuad q;
         incExprQuadLen ();
         esv
      | _ ->
         (* Constant *)
         let plc = const_binop (op, typ, c1, c2) in
         let esv = {
           e_place = plc;
           e_typ = typ
         } in esv

(* Semantic-Quad actions for relop operators *)
let sq_relop a op b x y =
    let typ = what_bin_type a op b in
    match typ with
    | TYPE_none ->
      (* if TYPE_none, we have an error *)
      binop_error a.e_typ op b.e_typ x y;
      esv_err
    | _ ->
      let tr = [!quadNext] in
      let q = Q_relop (op, a.e_place, b.e_place, Q_backpatch) in
      addNewQuad q;
      incExprQuadLen ();
      let fa = [!quadNext] in
      let q = Q_jump ( Q_backpatch ) in
      addNewQuad q;
      incExprQuadLen ();
      let csv = {
        c_true = tr;
        c_false = fa
      } in
      let e = expr_of_cond csv in
      e

(* Semantic-Quad actions for unary operators *)
let sq_unop op a x y =
    let typ = what_un_type op a in
    match typ with
    | TYPE_none ->
      (* if TYPE_none we have an error *)
      unop_error op a.e_typ x y;
      esv_err
    | _ ->
      let e = newTemporary typ in
      let q = Q_op (op, a.e_place, Q_dash, Q_entry e) in
      let esv = {
        e_place = (Q_entry e);
        e_typ = typ
      } in
      addNewQuad q;
      incExprQuadLen ();
      esv

(* Semantic-Quad actions for constant definiton *)
let sq_cdef n t v =
  try
    (* Lookup the Symbol Table. not_found is not handled *)
    let e = lookupEntry (id_make n) LOOKUP_CURRENT_SCOPE false in
      (* if found, name is already taken *)
      error "Const name %s is already taken" n;
      ignore(e)
  with Not_found ->
    (* if not found, check types *)
    if equalType t v.e_typ then
      (* if match, find the constant value which must be known at compile-time *)
      match const_of_quad v.e_place with
      | CONST_none ->
        (* not really a const *)
        error "%s is not really a const!" n
      | cv ->
        (* register the new Constant *)
        ignore (newConstant (id_make n) t cv false)
    else
      (* const def type mismatch *)
      error "constant definition type mismatch"

(* Semantic-Quad actions for lvalue *)
let sq_lvalue name idxs =
  (* a function that generates quads and temporaries for array indexing *)
  let rec mktemp id typ index_list =
    match typ, index_list with
    | TYPE_array(et, sz), h::t ->
       begin
       (* FIXME is temporary type TYPE_array OK ? *)
       let newt = newTemporary et in
       let q = Q_array (id, h.e_place, Q_entry newt) in
       addNewQuad q;
       incLvalQuadLen ();
       mktemp (Q_deref newt) et t
       end
    | _, [] ->
       let esv = {
         e_place = id;
         e_typ = typ
       } in esv
    | _ ->
       error "lvalue %s error!" name;
       esv_err
  in
  try
    (* Lookup the Symbol Table, do not handle the not_found case *)
    let e = lookupEntry (id_make name) LOOKUP_CURRENT_SCOPE false in
    (* check if entry is variable or parameter or constant *)
    match e.entry_info with
    | ENTRY_variable inf ->
        mktemp (Q_entry e) inf.variable_type idxs
    | ENTRY_parameter inf ->
        mktemp (Q_entry e) inf.parameter_type idxs
    | ENTRY_constant inf ->
        begin
        let qv = quad_of_const inf.constant_value in
        match qv with
        | Q_none ->
           error "lvalue %s is not really a const while it should be" name;
           esv_err
        | _ ->
           mktemp qv inf.constant_type idxs
        end
    | _ ->
        error "lvalue %s is not a variable or a parameter or a constant in the current scope!" name;
        esv_err
  with Not_found ->
    (* if not found in current scope, check if it's a constant in the global scope *)
    (* Lookup the Symbol Table, and handle the not_found case *)
    let e = lookupEntry (id_make name) LOOKUP_ALL_SCOPES true in
    match e.entry_info with
    | ENTRY_constant inf -> 
        begin
        let qv = quad_of_const inf.constant_value in
        match qv with
        | Q_none ->
           error "lvalue %s is not really a const while it should be" name;
           esv_err;
        | _ ->
           mktemp qv inf.constant_type idxs
        end
    | _ ->
        error "lvalue %s isn't a constant!" name;
        esv_err

let sq_assign a op b =
  (* FIXME check for type compatibility instead of type equality ? *)
  if a.e_typ = b.e_typ then
    match op with
    | "=" ->
      let q = Q_assign (b.e_place, a.e_place) in
      addNewQuad q;
      1
    | binop ->
      let e = sq_binop a binop b (rhs_start_pos 1) (rhs_end_pos 3) in
      let q = Q_assign (e.e_place, a.e_place) in
      addNewQuad q;
      1
  else
    0

(* Semantic-Quads actions for variable definition *)
let sq_vardef typ (name, dims, init) =
  try
    (* Lookup the Symbol Table, do not handle the not_found case *)
    let e = lookupEntry (id_make name) LOOKUP_CURRENT_SCOPE false in
      (* if found, name is already taken *)
      error "Var name %s is already taken" name;
      ignore(e);
  with Not_found ->
    (* if not found, name is available *)
    match init.e_place with
    | Q_none ->
      (* no initialization OR array *)
      (* ft is a function that produces the full type of an array *)
      let rec ft = function
      | (Q_int h)::t -> TYPE_array ( (ft t), h )
      | [] -> typ
      | _ -> error "Array dimension isn't int const"; TYPE_none
      in
      ignore( newVariable (id_make name) (ft dims) false );
    | _ ->
      (* initialization is present *)
      let n = newVariable (id_make name) typ false in
      let var = {
        e_place = Q_entry n;
        e_typ = typ
      } in
      let l = sq_assign var "=" init in
      if ( l = 1 ) then incExprQuadLen ()
      else if ( l > 0 ) then internal "l > 1"
      else ()

(* Semantic-Quads actions for routine header *)
let sq_rout_head name typ pars =
  (* register a new function or find the forwarded function header *)
  let e = newFunction (id_make name) true in
    openScope ();
    (* add parameters *)
    let paramadd (t, (n, m, dims)) =
      let rec ft = function
        | (Q_int d)::ds -> TYPE_array (ft ds, d)
        | [] -> t
        | _ -> TYPE_none
      in
        ignore (newParameter (id_make n) (ft dims) m e true)
    in
      let q = Q_unit (Q_entry e) in
      addNewQuad q;
      List.iter paramadd pars;
      endFunctionHeader e typ;
      e

(* Semantic-Quads actions for routine call *)
let sq_rout_call name pars =
  (* Lookup the Symbol Table, and handle the not_found case *)
  let e = lookupEntry (id_make name) LOOKUP_ALL_SCOPES true in
    match e.entry_info with
    (* e should be function entry *)
    | ENTRY_function inf ->
        begin
        (* should be PARDEF_COMPLETE *)
        match inf.function_pstatus with
        | PARDEF_COMPLETE ->
           begin
           (* a function that matches ARRAY actual parameters with ARRAY formal parameters *)
           let dimmatch formal actual =
           match formal, actual with
           | TYPE_array(et1,sz1), TYPE_array(et2, sz2) -> if (sz1 = 0 || sz1 = sz2) then equalArrayType et1 et2 else false
           | _ -> true
           in 
           (* function that matches a list of formal parameters with a list of actual parameters *)
           let rec parmatch = function
           | (fh::ft, ah::at) ->
              begin
              match fh.entry_info with
              | ENTRY_parameter inf ->
                 if (not (equalType inf.parameter_type ah.e_typ) || not (dimmatch inf.parameter_type ah.e_typ)) then
                   begin
                   error "Parameter type mismatch when calling %s" name;
                   false
                   end
                 else
                   let q = Q_par (ah.e_place, Q_pass_mode (quad_of_passmode inf.parameter_mode)) in
                   addNewQuad q;
                   parmatch (ft,at)
              | _ ->
                 error "what the hell? this ain't no parameter!";
                 false
              end
           | ([], []) -> true
           | ([], _)
           | (_, []) ->
              error "There are too many or not enough arguments when calling %s" name;
              false
           in
           (* do the matching *)
           if (parmatch (inf.function_paramlist, pars)) then
             (* matching done successfully! *)
             begin
             match inf.function_result with
             | TYPE_proc ->
               (* routine is a PROCEDURE, no result is expected to be returned, so just call *)
               let q = Q_call (Q_entry e) in
               addNewQuad q;
               let esv = {
                  e_place = Q_none;
                  e_typ = TYPE_proc
               } in esv
             | _ ->
               (* routine is a FUNCTION, so reserve memory for result, and then call *)
               let t = inf.function_result in
               let etemp = newTemporary t in
               let q = Q_par (Q_entry etemp, Q_pass_mode RET) in
               addNewQuad q;
               let q = Q_call (Q_entry e) in
               addNewQuad q;
               let esv = {
                 e_place = (Q_entry etemp);
                 e_typ = t
               } in esv
             end
           else
             (* some error occured and parmatch failed! *)
             esv_err
           end
        (* maybe should never reach *)
        | _ ->
           error "function params are not checked?";
           esv_err
        end
    (* should never reach *)
    | _ ->
        error "Cannot call %s, is not a function" name;
        esv_err

let sq_plus_plus id =
  let esv = {
    e_place = Q_int 1;
    e_typ = TYPE_int
  } in
  let e = sq_binop id "+" esv (rhs_start_pos 1) (rhs_end_pos 3) in
  let q = Q_assign (e.e_place, id.e_place) in
  [q]

let sq_minus_minus id =
  let esv = {
    e_place = Q_int 1;
    e_typ = TYPE_int
  } in
  let e = sq_binop id "-" esv (rhs_start_pos 1) (rhs_end_pos 3) in
  let q = Q_assign (e.e_place, id.e_place) in
  [q]
