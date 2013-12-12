(* Quadruples *)

open Printf
open Lexing
open Parsing

open Error
open Identifier
open Symbol
open Types



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

(* Used for simple cases *)
let ssv_empty = {
  s_next = [];
  (* s_code = [] *)
  s_len = 0
}

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

(* convert Symbol.pass_mode to SemQuad.quad_pass_mode *)
let quad_of_passmode = function
  | PASS_BY_VALUE -> V
  | PASS_BY_REFERENCE -> R

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

(* convert SemQuad.quad_op_t to string *)
let string_of_quad_op = function
  | Q_none -> "<none>"
  | Q_int i -> string_of_int i
  | Q_real r -> string_of_float r
  | Q_bool b -> string_of_bool b
  | Q_char c -> sprintf "%C" c (* Char.escaped c *)
  | Q_string s -> s
  | Q_entry e -> id_name e.entry_id
  | Q_funct_res -> "$$"
  | Q_deref e -> "[" ^ id_name e.entry_id ^ "]"
  | Q_lbl l -> string_of_int l
  | Q_pass_mode V -> "V"
  | Q_pass_mode R -> "R"
  | Q_pass_mode RET -> "RET"
  | Q_dash -> "--"
  | Q_backpatch -> "*"
  | _ -> "<stub!>"

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
  | Q_jumpl l ->            concat4 "jumpl" Q_dash  Q_dash  l
  | Q_call u ->             concat4 "call"  Q_dash  Q_dash  u
  | Q_par (x,m) ->          concat4 "par"   x       m       Q_dash
  | Q_ret ->                concat4 "ret"   Q_dash  Q_dash  Q_dash



(* Intermediate Code Representation *)

(* intermediate code is a list of quads *)
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

(* remove from quad list reference lis the quad with FIXME *)
let rmQuad lis n =
  let rec seekAndDestroy a b which =
    match b with
    | h::t ->
      if ( h = which ) then
        begin
        !lis <- a@t
        end
      else
        seekAndDestroy (a@[h]) t which
    | _ -> internal "quad asked to be removed does not exist!"
  in
  seekAndDestroy [] !lis n

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
    | _ -> internal "error! cannot backpatch that quad! %s\n" (string_of_quad quad); raise Exit
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



(* Quads produced by expressions *)

(* how many quads have been produced, due to expressions? *)
let exprQuadLen = ref 0

(* reset counter. called after "reading" !exprQuadLen *)
let resetExprQuadLen () =
  !exprQuadLen <- 0

(* increase counter *)
let incExprQuadLen () =
  !exprQuadLen <- !exprQuadLen + 1



(* Quads produced by lvalue *)

(* how many quads have been produced, due to lvalue? *)
let lvalQuadLen = ref 0

(* reset counter. called after "reading" !lvalQuadLen *)
let resetLvalQuadLen () =
  !lvalQuadLen <- 0

(* increase counter *)
let incLvalQuadLen () =
  !lvalQuadLen <- !lvalQuadLen + 1



(* Quads produced by routines *)

(* how many quads have been produced, due to routines? *)
let routQuadLen = ref 0

(* reset counter. called after "reading" !lvalQuadLen *)
let resetRoutQuadLen () =
  !routQuadLen <- 0

(* increase counter *)
let incRoutQuadLen () =
  !routQuadLen <- !routQuadLen + 1

(* Lemonida's error handling *)

(*
(* Simple Function to get Expression Position *)
let get_binop_pos () =
    (rhs_start_pos 1, rhs_start_pos 3)

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



(* Error Handling *)

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



(* Some kind of Optimization for constants *)

let const_unop = function
  (* add *)
  | "+", TYPE_int, CONST_int x -> Q_int x
  | "+", TYPE_int, CONST_char x -> Q_int (int_of_char x)
  | "+", TYPE_REAL, CONST_REAL x -> Q_real x
  (* sub *)
  | "-", TYPE_int, CONST_int x -> Q_int (-x)
  | "-", TYPE_int, CONST_char x -> Q_int (-(int_of_char x))
  | "-", TYPE_REAL, CONST_REAL x -> Q_real (-.x)
  (* not *)
  | "!", TYPE_bool, CONST_bool x -> Q_bool (not x)
  (* anything else *)
  | _ -> Q_none
  

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



(* Semantic Actions and Icode generation *)

(* Semantic-Quad actions for constant definiton *)
let sq_cdef name typ value =
  try
    (* Lookup the Symbol Table. not_found is not handled *)
    let e = lookupEntry (id_make name) LOOKUP_CURRENT_SCOPE false in
      (* if found, name is already taken *)
      error "Const name %s is already taken" name;
      ignore(e)
  with Not_found ->
    (* if not found, check types *)
    if equalType typ value.e_typ then
      (* if match, find the constant value which must be known at compile-time *)
      match const_of_quad value.e_place with
      | CONST_none ->
        (* not really a const *)
        error "%s is not really a const!" name
      | const_value ->
        (* register the new Constant *)
        ignore (newConstant (id_make name) typ const_value false)
    else
      (* const def type mismatch *)
      error "constant definition type mismatch"


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

(* Semantic-Quad actions for unary operators *)
let sq_unop op a x y =
    let typ = what_un_type op a in
    match typ with
    | TYPE_none ->
      (* if TYPE_none we have an error *)
      unop_error op a.e_typ x y;
      esv_err
    | _ ->
      (* Check if Constants *)
      let c1 = const_of_quad a.e_place in
      match c1 with
      | CONST_none ->
        (* No constant *)
        (* make new temporary *)
        let e = newTemporary typ in
        (* generate quad *)
        let q = Q_op (op, a.e_place, Q_dash, Q_entry e) in
        (* return expression semantic value *)
        let esv = {
          e_place = (Q_entry e);
          e_typ = typ
        } in
        addNewQuad q;
        (* a quad has been added to icode, due to expression *)
        incExprQuadLen ();
        esv
      | _ ->
        (* Constant *)
        let plc = const_unop (op, typ, c1) in
        let esv = {
          e_place = plc;
          e_typ = typ
        } in esv


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
      let c1 = const_of_quad a.e_place in
      let c2 = const_of_quad b.e_place in
      match c1,c2 with
      | CONST_none, _
      | _, CONST_none ->
        (* No constant *)
        (* make new temporary *)
        let e = newTemporary typ in
        (* generate quad *)
        let q = Q_op (op, a.e_place, b.e_place, Q_entry e) in
        (* return expression semantic value *)
        let esv = {
          e_place = (Q_entry e);
          e_typ = typ
        } in
        addNewQuad q;
        (* a quad has been added to icode, due to expression *)
        incExprQuadLen ();
        esv
      | _ ->
        (* Constant *)
        let plc = const_binop (op, typ, c1, c2) in
        let esv = {
          e_place = plc;
          e_typ = typ
        } in esv

(* make a condition out of an expression *)
let cond_of_expr e =
  (* list of "true" quads; to be backpatched *)
  let tr = [!quadNext] in
  let q1 = Q_ifb (e.e_place, Q_backpatch) in
  addNewQuad q1;
  (* list of "false" quads; to be backpatched *)
  let fa = [!quadNext] in
  let q2 = Q_jump (Q_backpatch) in
  addNewQuad q2;
  (* return the condition *)
  let c = {
    c_true = tr;
    c_false = fa
  } in c

(* make an expression out of a condition *)
let expr_of_cond c =
  (* make a new bool temporary to hold condition's value *)
  let e = newTemporary TYPE_bool in
  (* start to make the "true" quads *)
  backpatch c.c_true (Q_int !quadNext);
  let q = Q_assign ( Q_bool true, Q_entry e) in
  addNewQuad q;
  (* a quad has been added to icode due to an expression *)
  incExprQuadLen ();
  (* generate the *fly-over-the-false-part* quad *)
  let foo = !quadNext + 2 in
  let q = Q_jump ( Q_int foo ) in
  addNewQuad q;
  (* a quad has been added to icode due to an expression *)
  incExprQuadLen ();
  (* start to make the "false" quads *)
  backpatch c.c_false (Q_int !quadNext);
  let q = Q_assign ( Q_bool false, Q_entry e) in
  addNewQuad q;
  (* a quad has been added to icode due to an expression *)
  incExprQuadLen ();
  (* return the expression semantic value *)
  let esv = {
    e_place = Q_entry e;
    e_typ = TYPE_bool
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
      (* list of "true" quads; to be backpatched *)
      let tr = [!quadNext] in
      let q = Q_relop (op, a.e_place, b.e_place, Q_backpatch) in
      addNewQuad q;
      (* a quad has been added to icode due to an expression *)
      incExprQuadLen ();
      (* list of "false" quads; to be backpatched *)
      let fa = [!quadNext] in
      let q = Q_jump ( Q_backpatch ) in
      addNewQuad q;
      (* a quad has been added to icode due to an expression *)
      incExprQuadLen ();
      (* make the condition... *)
      let csv = {
        c_true = tr;
        c_false = fa
      } in
      (* ...and generate an expression out of it *)
      let e = expr_of_cond csv in
      (* return an expression *)
      e


(* Semantic-Quad actions for lvalue *)
let sq_lvalue name idxs =
  (* a function that generates quads and temporaries for array indexing *)
  let rec mkArrTemp id typ index_list =
    match typ, index_list with
    | TYPE_array(et, sz), h::t ->
       begin
       (* FIXME is temporary type TYPE_array OK ? *)
       let newt = newTemporary et in
       let q = Q_array (id, h.e_place, Q_entry newt) in
       addNewQuad q;
       (* a quad has been added to icode, due to lvalue *)
       incLvalQuadLen ();
       mkArrTemp (Q_deref newt) et t
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
        mkArrTemp (Q_entry e) inf.variable_type idxs
    | ENTRY_parameter inf ->
        mkArrTemp (Q_entry e) inf.parameter_type idxs
    | ENTRY_constant inf ->
        begin
        let qv = quad_of_const inf.constant_value in
        match qv with
        | Q_none ->
           error "lvalue '%s' is not really a const while it should be" name;
           esv_err
        | _ ->
           mkArrTemp qv inf.constant_type idxs
        end
    | _ ->
        error "lvalue '%s' is not a variable or a parameter or a constant in the current scope!" name;
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
           error "lvalue '%s' is not really a const while it should be" name;
           esv_err
        | _ ->
           mkArrTemp qv inf.constant_type idxs
        end
    | _ ->
        error "lvalue '%s' isn't a constant!" name;
        esv_err

let sq_assign a op b =
  (* FIXME check for type compatibility instead of type equality ? *)
  if a.e_typ = b.e_typ then
    match op with
    | "=" ->
      let q = Q_assign (b.e_place, a.e_place) in
      addNewQuad q;
      (* a quad has been added to icode, due to expression *)
      incExprQuadLen ()
    | binop ->
      let e = sq_binop a binop b (rhs_start_pos 1) (rhs_end_pos 3) in
      let q = Q_assign (e.e_place, a.e_place) in
      addNewQuad q;
      (* a quad has been added to icode, due to expression *)
      incExprQuadLen ()
  else
    error "type mismatch when assigning a value to '%s'" (string_of_quad_op a.e_place);
    ()

(* Semantic-Quads actions for variable definition *)
let sq_vardef typ (name, dims, init) =
  try
    (* Lookup the Symbol Table, do not handle the not_found case *)
    let e = lookupEntry (id_make name) LOOKUP_CURRENT_SCOPE false in
      (* if found in Current Scope, name is already taken *)
      error "Var name '%s' is already taken" name;
      ignore e
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
      ignore ( newVariable (id_make name) (ft dims) false )
    | _ ->
      (* initialization is present *)
      let n = newVariable (id_make name) typ false in
      let var = {
        e_place = Q_entry n;
        e_typ = typ
      } in
      sq_assign var "=" init

(* Semantic-Quads actions for routine header *)
let sq_rout_head name typ pars =
  (* register a new function or find the forwarded function header *)
  let e = newFunction (id_make name) true in
  openScope ();
  (* add parameters *)
  let parameterAdd (param_type, (param_name, param_pass_mode, param_dimensions)) =
    let rec fullType = function
      | (Q_int d)::ds -> TYPE_array (fullType ds, d)
      | [] -> param_type
      | _ -> TYPE_none
    in
      ignore (newParameter (id_make param_name) (fullType param_dimensions) param_pass_mode e true)
  in
    let q = Q_unit (Q_entry e) in
    addNewQuad q;
    List.iter parameterAdd pars;
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
                   if ( (not (equalType inf.parameter_type ah.e_typ)) || (not (dimmatch inf.parameter_type ah.e_typ)) ) then
                     begin
                     error "Parameter type mismatch when calling '%s'" name;
                     false
                     end
                   else
                     let q = Q_par (ah.e_place, Q_pass_mode (quad_of_passmode inf.parameter_mode)) in
                     addNewQuad q;
                     (* a quad has beed added to icode due to routine *)
                     incRoutQuadLen ();
                     parmatch (ft,at)
                | _ ->
                   error "what the hell? this ain't no parameter!";
                   false
                end
             | ([], []) -> true
             | ([], _)
             | (_, []) ->
                error "There are too many or not enough arguments when calling '%s'" name;
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
               (* a quad has beed added to icode due to routine *)
               incRoutQuadLen ();
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
               (* a quad has beed added to icode due to routine *)
               incRoutQuadLen ();
               let q = Q_call (Q_entry e) in
               addNewQuad q;
               (* a quad has beed added to icode due to routine *)
               incRoutQuadLen ();
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
  let one = {
    e_place = Q_int 1;
    e_typ = TYPE_int
  } in
  let e = sq_binop id "+" one (rhs_start_pos 1) (rhs_end_pos 3) in
  let q = Q_assign (e.e_place, id.e_place) in
  addNewQuad q;
  incExprQuadLen ()

let sq_minus_minus id =
  let one = {
    e_place = Q_int 1;
    e_typ = TYPE_int
  } in
  let e = sq_binop id "-" one (rhs_start_pos 1) (rhs_end_pos 3) in
  let q = Q_assign (e.e_place, id.e_place) in
  addNewQuad q;
  incExprQuadLen ()



(* Break *)

let breakQuad = ref []

let addBreakQuad l =
  !breakQuad <- !breakQuad @ l

let resetBreakQuad () =
  !breakQuad <- []

let collectMyBreaks a b =
  let belongsToMe n = 
    if ( a < n && n < b ) then true else false
  in
  let fix n =
    if ( belongsToMe n ) then
      begin
      backpatch [n] (Q_int b);
      rmQuad breakQuad n;
      end
    else
      ()
  in
  List.iter fix !breakQuad



(* Continue *)

let contQuad = ref []

let addContQuad l =
  !contQuad <- !contQuad @ l

let resetContQuad () =
  !contQuad <- []

let collectMyConts a b =
  let belongsToMe n = 
    if ( a < n && n < b ) then true else false
  in
  let fix n =
    if ( belongsToMe n ) then
      begin
      backpatch [n] (Q_int a);
      rmQuad contQuad n;
      end
    else
      ()
  in
  List.iter fix !contQuad



(* FOR loop range *)

let sq_range a b c =
  match a.e_typ, b.e_typ, c.e_typ with
  | TYPE_int, TYPE_int, TYPE_int ->
    (a.e_place, b.e_place, c.e_place)
  | _ ->
    error "Range arguments isn't all ints!";
    let zero = Q_int 0 in
    (zero, zero, zero)

let sq_for_control i a b c =
  let ie = sq_lvalue i [] in
  let q1 = Q_assign (a, ie.e_place) in
  addNewQuad q1;
  incExprQuadLen ();
  let len0 = !exprQuadLen in
  resetExprQuadLen ();
  if (debug) then printf "\tafter init, quads = %d\n" len0;
  let cmp = if ( a <= b ) then "<" else ">" in
  let tr = [!quadNext] in
  let q2 = Q_relop (cmp, ie.e_place, b, Q_backpatch) in
  addNewQuad q2;
  incExprQuadLen ();
  let fa = [!quadNext] in
  let q3 = Q_jump (Q_backpatch) in
  addNewQuad q3;
  incExprQuadLen ();
  let csv = {
    c_true = tr;
    c_false = fa
  } in
  let e = expr_of_cond csv in
  let cond = cond_of_expr e in
  let cqs = 2 in
  let l1 = !exprQuadLen in
  resetExprQuadLen ();
  let l2 = !lvalQuadLen in
  resetLvalQuadLen ();
  if (debug) then printf "\tafter check, quads = %d\n" (cqs+l1+l2);
  let len = len0 + cqs + l1 + l2 in
  let e = newTemporary TYPE_int in
  let q4 = Q_op ("+", ie.e_place, c, Q_entry e) in
  let q5 = Q_assign (Q_entry e, ie.e_place) in
  let incquads = q4::[q5] in
  (len0, cond, len, incquads)

let sq_format (x, w, d) =
  let check () =
    if (w.e_typ != TYPE_int) then
      begin
      error "Printable characters number is not an int";
      false
      end
    else if (d.e_typ != TYPE_int) then
      begin
      error "Printable decimals number is not an int";
      false
      end
    else
      match d.e_place, x.e_typ with
      | Q_none, _ -> true
      | a, TYPE_REAL -> true
      | _ -> false
      (*
        if (a = 1) then
          begin
          error "Printable decimals setting is present, but expression to be printed isn't of type REAL";
          (x,w,d)
          end
        else
          (x,w,d)
    | _ -> (x,w,d)
    *)
  in
  if ( check () ) then
    (* (x.e_place, w.e_place, d.e_place) *)
    (x,w,d)
  else
    begin
    error "FORMAT error";
    (* (Q_none, Q_none, Q_none) *)
    (esv_err, esv_err, esv_err)
    end



(* Statements *)

let st_block stmt1 stmt2 =
  (* first statement's next is second statement's beginning *)
  backpatch stmt1.s_next (Q_int (!quadNext - stmt2.s_len));
  (* return a statement semantic value *)
  let ssv = {
    s_next = stmt2.s_next;
    s_len = stmt1.s_len + stmt2.s_len
  } in ssv

let st_constdef () =
  (* no quads are generated for constant definition *)
  ssv_empty

let st_vardef () =
  (* some quads may have been added due to variable initialization *)
  let l1 = !exprQuadLen in
  resetExprQuadLen ();
  (* return a statement semantic value *)
  let ssv = {
    s_next = [];
    s_len = l1
  } in ssv

let st_cond_of_expr e =
  (* Convert an expression to a condition *)
  let c = cond_of_expr e in
  (* "if something jump there, else jump there" is 2 quads long *)
  let qs = 2 in
  (* collect quads generated due to expression *)
  let l1 = !exprQuadLen in
  resetExprQuadLen ();
  (* collect quads generated due to lvalue *)
  let l2 = !lvalQuadLen in
  resetLvalQuadLen ();
  (* compute total length *)
  let len = qs + l1 + l2 in
  if (debug) then printf "conversion of expression to condition is %d quads long\n" len;
  (c,len)

let st_fly () =
  (* just add a jump quad to fly over the "else" part *)
  let l = [!quadNext] in
  let q = Q_jump ( Q_backpatch ) in
  addNewQuad q;
  (l,1)

let st_assign lval op exp =
  (* generate the assignment quads *)
  sq_assign lval op exp;
  let l1 = !exprQuadLen in
  (* collect quads generated due to expression *)
  resetExprQuadLen ();
  let l2 = !lvalQuadLen in
  (* collect quads generated due to lvalue *)
  resetLvalQuadLen ();
  let ssv = {
    s_next = [];
    s_len = l1 + l2
  } in ssv

let st_plusplus lval =
  (* generate the assignment quads *)
  sq_plus_plus lval;
  let l1 = !lvalQuadLen in
  (* collect quads generated due to lvalue *)
  resetLvalQuadLen ();
  let l2 = !exprQuadLen in
  (* collect quads generated due to expression *)
  resetExprQuadLen ();
  let ssv = {
    s_next = [];
    s_len = l1 + l2
  } in ssv

let st_minusminus lval =
  (* generate the assignment quads *)
  sq_minus_minus lval;
  let l1 = !lvalQuadLen in
  (* collect quads generated due to lvalue *)
  resetLvalQuadLen ();
  let l2 = !exprQuadLen in
  (* collect quads generated due to expression *)
  resetExprQuadLen ();
  let ssv = {
    s_next = [];
    s_len = l1 + l2
  } in ssv

let st_call () =
  let l1 = !lvalQuadLen in
  (* collect quads generated due to lvalue *)
  resetLvalQuadLen ();
  let l2 = !exprQuadLen in
  (* collect quads generated due to expression *)
  resetExprQuadLen ();
  let l3 = !routQuadLen in
  (* collect quads generated due to routine *)
  resetRoutQuadLen ();
  let len = l1 + l2 + l3 in
  if (debug) then printf "Call is %d long\n" len;
  let ssv = {
    s_next = [];
    s_len = len
  } in ssv

let st_if_then cond stmt =
  (* handle if then *)
  let (c,qs) = cond in
  if (debug) then printf "if-then->cond is %d quad long\n" qs;
  if (debug) then printf "if-then->stmt is %d quad long\n" stmt.s_len;
  (* if condition is true, jump to the statement *)
  backpatch c.c_true (Q_int (!quadNext - stmt.s_len));
  (* merge cond.false and stmt.next as if_then.next *)
  let l1 = c.c_false in
  let l = List.merge compare l1 stmt.s_next in
  let len = qs + stmt.s_len in
  if (debug) then printf "if-then is %d quad long\n" len;
  let ssv = {
    s_next = l;
    s_len = len
  } in ssv

let st_if_then_else cond stmt1 fly stmt2 =
  (* handle if then else *)
  let (c,qs) = cond in
  let (fly_back, fly_q) = fly in
  if (debug) then printf "if-then-else->cond is %d quad long\n" qs;
  if (debug) then printf "if-then-else->stmt1 is %d quad long\n" stmt1.s_len;
  if (debug) then printf "if-then-else->stmt2 is %d quad long\n" stmt2.s_len;
  (* if condition is true, jump to first statement *)
  backpatch c.c_true (Q_int (!quadNext -stmt2.s_len -1 -stmt1.s_len));
  (* if condition is false, jump to the second statement *)
  backpatch c.c_false (Q_int (!quadNext - stmt2.s_len));
  (* merge stmt1.next, fly and stmt2.next as if_then_else.next *)
  let l2 = List.merge compare fly_back stmt1.s_next in
  let l = List.merge compare l2 stmt2.s_next in
  let len = qs + stmt2.s_len + fly_q + stmt2.s_len in
  if (debug) then printf "if-then-else is %d quad long\n" len;
  let ssv = {
    s_next = l;
    s_len = len
  } in ssv

let st_while cond stmt =
  (* handle while loop *)
  let (c,qs) = cond in
  if (debug) then printf "while->stmt is %d quads long\n" stmt.s_len;
  let stmt_start = !quadNext - stmt.s_len in
  (* if condition is true jump to statement *)
  backpatch c.c_true (Q_int stmt_start);
  (* set stmt.next to be the jump_to_condition quad *)
  backpatch stmt.s_next (Q_int (stmt_start - qs));
  (* FIXME maybe -> backpatch $5.s_next (Q_int !quadNext); *)
  let q = Q_jump (Q_int (stmt_start - qs)) in
  addNewQuad q;
  (* find and fix any breaks associated with this while loop *)
  collectMyBreaks (stmt_start - qs) (!quadNext);
  (* find and fix any continues associated with this while loop *)
  collectMyConts (stmt_start - qs) (!quadNext);
  let ssv = {
    s_next = c.c_false;
    s_len = qs + stmt.s_len + 1
  } in ssv

let st_for control stmt =
  (* handle for loop *)
  (* FIXME what about messing with the iterator? *)
  let (init, c, qs, stepqs) = control in
  let stmt_start = !quadNext - stmt.s_len in
  (* if condition is true, jump to the statement *)
  backpatch c.c_true (Q_int stmt_start);
  (* set stmt.next to be the step quads *)
  backpatch stmt.s_next (Q_int !quadNext);
  (* NOW is the time to add the step quads *)
  List.iter addNewQuad stepqs;
  let steplen = List.length stepqs in
  (* jump to condition *)
  let q = Q_jump (Q_int (stmt_start - qs + init)) in
  addNewQuad q;
  (* find and fix any breaks associated with this for loop *)
  collectMyBreaks (stmt_start - qs) (!quadNext);
  (* find and fix any continues associated with this for loop *)
  collectMyConts (stmt_start - qs + init) (!quadNext);
  let ssv = {
    s_next = c.c_false;
    s_len = qs + stmt.s_len + steplen + 1
  } in ssv

let st_do_while stmt cond =
  (* handle do-while *)
  let (c,qs) = cond in
  let stmt_start = !quadNext -stmt.s_len -qs in
  (* if condition is true, jump back to the statement *)
  backpatch c.c_true (Q_int stmt_start);
  (* set stmt.next to be the condition *)
  backpatch stmt.s_next (Q_int (stmt_start + stmt.s_len));
  (* find and fix any breaks associated with this do-while loop *)
  collectMyBreaks (stmt_start - qs) (!quadNext);
  (* find and fix any continues associated with this do-while loop *)
  collectMyConts (stmt_start) (!quadNext);
  let ssv = {
    s_next = c.c_false;
    s_len = stmt.s_len + qs
  } in ssv

let st_break () =
  (* break *)
  let brk = [!quadNext] in
  let q = Q_jump (Q_backpatch) in
  addNewQuad q;
  (* register the break *)
  addBreakQuad brk;
  let ssv = {
    s_next = [];
    s_len = 1
  } in ssv

let st_continue () =
  (* continue *)
  let cnt = [!quadNext] in
  let q = Q_jump (Q_backpatch) in
  addNewQuad q;
  (* register the continue *)
  addContQuad cnt;
  let ssv = {
    s_next = [];
    s_len = 1
  } in ssv

let st_return_simple () =
  (* handle return *)
  let q = Q_ret in
  addNewQuad q;
  let ssv = {
    s_next = [];
    s_len = 1
  } in ssv

let st_return e =
  (* handle return *)
  let q1 = Q_assign (e.e_place , Q_funct_res) in
  let q2 = Q_ret in
  addNewQuad q1;
  addNewQuad q2;
  let ssv = {
    s_next = [];
    s_len = 2
  } in ssv

let st_write w l =
(*
    | "WRITE" -> 0 (* do nothing *)
    | "WRITESP" -> 1 (* add spaces *)
    | "WRITELN" -> 2 (* add a new line @ the end *)
    | "WRITESPLN" -> 3 (* add both spaces and a new line @ the end *)
*)
  let gen_quads a b c =
    match a.e_place with
    | Q_int i ->
      (* integer constant *)
      ignore (sq_rout_call "writeInteger" [a]);
      let l1 = !routQuadLen in
      resetRoutQuadLen ();
      l1
    | Q_char c ->
      (* character constant *)
      ignore (sq_rout_call "writeChar" [a]);
      let l1 = !routQuadLen in
      resetRoutQuadLen ();
      l1
    | Q_bool b ->
      (* boolean constant *)
      ignore (sq_rout_call "writeBoolean" [a]);
      let l1 = !routQuadLen in
      resetRoutQuadLen ();
      l1
    | Q_real r ->
      (* REAL constant *)
      ignore (sq_rout_call "writeReal" [a]);
      let l1 = !routQuadLen in
      resetRoutQuadLen ();
      l1
    | Q_string s ->
      (* string constant *)
      ignore (sq_rout_call "writeString" [a]);
      let l1 = !routQuadLen in
      resetRoutQuadLen ();
      l1
    | Q_entry e ->
      begin
      let typ = match e.entry_info with
      | ENTRY_variable inf -> inf.variable_type
      | ENTRY_parameter inf -> inf.parameter_type
      | ENTRY_constant inf -> inf.constant_type
      | _ -> TYPE_none
      in
      match typ with
      | TYPE_int ->
        (* integer *)
        ignore (sq_rout_call "writeInteger" [a]);
        let l1 = !routQuadLen in
        resetRoutQuadLen ();
        l1
      | TYPE_char ->
        (* character constant *)
        ignore (sq_rout_call "writeChar" [a]);
        let l1 = !routQuadLen in
        resetRoutQuadLen ();
        l1
      | TYPE_bool ->
        (* boolean constant *)
        ignore (sq_rout_call "writeBoolean" [a]);
        let l1 = !routQuadLen in
        resetRoutQuadLen ();
        l1
      | TYPE_REAL ->
        (* REAL constant *)
        ignore (sq_rout_call "writeReal" [a]);
        let l1 = !routQuadLen in
        resetRoutQuadLen ();
        l1
      | TYPE_array(TYPE_char, _) ->
        (* string constant *)
        ignore (sq_rout_call "writeString" [a]);
        let l1 = !routQuadLen in
        resetRoutQuadLen ();
        l1
      | _ -> 0
      end
    | _ ->
      0
  in
  let gen_sp () =
    if (w = 1 || w = 3) then
      begin
      let space = {
        e_place = Q_char ' ';
        e_typ = TYPE_char
      } in
      ignore (sq_rout_call "writeChar" [space]);
      let l1 = !routQuadLen in
      resetRoutQuadLen ();
      l1
      end
    else
      0
  in
  let rm_last_sp () =
    if (w = 1 || w = 3) then
      begin
      rmLastQuad ();
      rmLastQuad ();
      end
  in
  let gen_nl () =
    if (w = 2 || w = 3) then
      begin
      let space = {
        e_place = Q_char '\n';
        e_typ = TYPE_char
      } in
      ignore (sq_rout_call "writeChar" [space]);
      let l1 = !routQuadLen in
      resetRoutQuadLen ();
      l1
      end
    else
      0
  in
  let rec gen_code = function
  | (a,b,c)::t ->
    let l1 = gen_quads a b c in
    let l2 = gen_sp () in
    l1 + l2 + gen_code t
  | [] ->
    rm_last_sp ();
    let l1 = gen_nl () in
    l1
  in
  let count =
    gen_code l
    (* List.fold_left (+) 0 (List.map gen_code l) *)
  in
  let ssv = {
    s_next = [];
    s_len = count
  } in ssv



(* Runtime Library *)

let register_runtime_library () =
  (* Register Writes *)
  ignore (
    sq_rout_head
    "writeInteger"
    TYPE_proc
    [ (TYPE_int, ("i", PASS_BY_VALUE, [])) ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "writeChar"
    TYPE_proc
    [ (TYPE_char, ("c", PASS_BY_VALUE, [])) ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "writeBoolean"
    TYPE_proc
    [ (TYPE_bool, ("b", PASS_BY_VALUE, [])) ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "writeReal"
    TYPE_proc
    [ (TYPE_REAL, ("r", PASS_BY_VALUE, [])) ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "writeString"
    TYPE_proc
    [ (TYPE_char, ("s", PASS_BY_REFERENCE, [Q_int 0])) ]
    );
  rmLastQuad ();

  (* Register STD OUT *)
  ignore (
    sq_rout_head
    "putchar"
    TYPE_proc
    [ (TYPE_char, ("c", PASS_BY_VALUE, [])) ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "puts"
    TYPE_proc
    [ (TYPE_char, ("s", PASS_BY_REFERENCE, [Q_int 0])) ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "WRITE_INT"
    TYPE_proc
    [ (TYPE_int, ("n", PASS_BY_VALUE, [])); (TYPE_int, ("w", PASS_BY_VALUE, [])) ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "WRITE_BOOL"
    TYPE_proc
    [ (TYPE_bool, ("b", PASS_BY_VALUE, [])); (TYPE_int, ("w", PASS_BY_VALUE, [])) ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "WRITE_CHAR"
    TYPE_proc
    [ (TYPE_char, ("c", PASS_BY_VALUE, [])); (TYPE_int, ("w", PASS_BY_VALUE, [])) ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "WRITE_REAL"
    TYPE_proc
    [ (TYPE_REAL, ("r", PASS_BY_VALUE, [])); (TYPE_int, ("w", PASS_BY_VALUE, [])); (TYPE_int, ("d", PASS_BY_VALUE, [])) ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "WRITE_STRING"
    TYPE_proc
    [ (TYPE_char, ("s", PASS_BY_REFERENCE, [Q_int 0])); (TYPE_int, ("w", PASS_BY_VALUE, [])) ]
    );
  rmLastQuad ();

  (* Register STD IN *)
  ignore (
    sq_rout_head
    "READ_INT"
    TYPE_int
    [ ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "READ_BOOL"
    TYPE_bool
    [ ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "getchar"
    TYPE_int
    [ ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "READ_REAL"
    TYPE_REAL
    [ ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "READ_STRING"
    TYPE_proc
    [ (TYPE_int, ("size", PASS_BY_VALUE, [])); TYPE_char, ("s", PASS_BY_REFERENCE, [Q_int 0])]
    );
  rmLastQuad ();

  (* Register STD LIB *)
  (* math *)
  ignore (
    sq_rout_head
    "abs"
    TYPE_int
    [ (TYPE_int, ("n", PASS_BY_VALUE, [])) ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "arctan"
    TYPE_REAL
    [ (TYPE_REAL, ("r", PASS_BY_VALUE, []))]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "cos"
    TYPE_REAL
    [ (TYPE_REAL, ("r", PASS_BY_VALUE, []))]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "exp"
    TYPE_REAL
    [ (TYPE_REAL, ("r", PASS_BY_VALUE, []))]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "fabs"
    TYPE_REAL
    [ (TYPE_REAL, ("r", PASS_BY_VALUE, []))]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "ln"
    TYPE_REAL
    [ (TYPE_REAL, ("r", PASS_BY_VALUE, []))]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "pi"
    TYPE_REAL
    [ ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "sin"
    TYPE_REAL
    [ (TYPE_REAL, ("r", PASS_BY_VALUE, []))]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "sqrt"
    TYPE_REAL
    [ (TYPE_REAL, ("r", PASS_BY_VALUE, []))]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "tan"
    TYPE_REAL
    [ (TYPE_REAL, ("r", PASS_BY_VALUE, []))]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "trunc"
    TYPE_REAL
    [ (TYPE_REAL, ("r", PASS_BY_VALUE, []))]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "round"
    TYPE_REAL
    [ (TYPE_REAL, ("r", PASS_BY_VALUE, []))]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "TRUNC"
    TYPE_int
    [ (TYPE_REAL, ("r", PASS_BY_VALUE, []))]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "ROUND"
    TYPE_int
    [ (TYPE_REAL, ("r", PASS_BY_VALUE, []))]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "strlen"
    TYPE_int
    [ (TYPE_char, ("s", PASS_BY_REFERENCE, [Q_int 0])) ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "strcmp"
    TYPE_int
    [ (TYPE_char, ("s1", PASS_BY_REFERENCE, [Q_int 0])); (TYPE_char, ("s2", PASS_BY_REFERENCE, [Q_int 0])) ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "strcpy"
    TYPE_proc
    [ (TYPE_char, ("trg", PASS_BY_REFERENCE, [Q_int 0])); (TYPE_char, ("src", PASS_BY_REFERENCE, [Q_int 0])) ]
    );
  rmLastQuad ();

  ignore (
    sq_rout_head
    "strcat"
    TYPE_proc
    [ (TYPE_char, ("trg", PASS_BY_REFERENCE, [Q_int 0])); (TYPE_char, ("src", PASS_BY_REFERENCE, [Q_int 0])) ]
    );
  rmLastQuad ()

