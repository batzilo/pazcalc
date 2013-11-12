(* Quadruples *)

open Printf
open Lexing
open Parsing

open Error
open Identifier
open Symbol
open Types

(* Quad operand datatype *)
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

let quad_t_of_pass_mode = function
  | PASS_BY_VALUE -> V
  | PASS_BY_REFERENCE -> R

let string_of_quad_op = function
  | Q_none -> ""
  | Q_int i -> string_of_int i
  | Q_real r -> string_of_float r
  | Q_bool b -> string_of_bool b
  | Q_char c -> Char.escaped c
  | Q_string s -> s
  | Q_entry e -> id_name e.entry_id
  | Q_deref e -> "[" ^ id_name e.entry_id ^ "]"
  | Q_label l -> string_of_int l
  | Q_pass_mode V -> "V"
  | Q_pass_mode R -> "R"
  | Q_pass_mode RET -> "RET"
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

let string_of_quad = function
  | Q_empty -> ""
  | Q_unit u -> "unit, " ^ string_of_quad_op u ^ ", -, -"
  | Q_endu u -> "endu, " ^ string_of_quad_op u ^ ", -, -"
  | Q_op (op,x,y,z) -> op ^ ", " ^ string_of_quad_op x ^ ", " ^ string_of_quad_op y ^ ", " ^ string_of_quad_op z
  | Q_assign (x,z) -> ":=, " ^ string_of_quad_op x ^ ", -, " ^ string_of_quad_op z
  | Q_array (x,y,z) -> "array, " ^ string_of_quad_op x ^ ", " ^ string_of_quad_op y ^ ", " ^ string_of_quad_op z
  | Q_relop (op,x,y,z) -> op ^ ", " ^ string_of_quad_op x ^ ", " ^ string_of_quad_op y ^ ", " ^ string_of_quad_op z
  | Q_ifb (x,z) -> "ifb, " ^ string_of_quad_op x ^ " ,- ," ^ string_of_quad_op z
  | Q_jump z -> "jump, -, -, " ^ string_of_quad_op z
  | Q_label l -> "unit, " ^ string_of_quad_op l ^ " ,- ,-"
  | Q_jl l -> "jump, -, -, " ^ string_of_quad_op l
  | Q_call u -> "call, -, -, " ^ string_of_quad_op u
  | Q_par (x,m) -> "par, " ^ string_of_quad_op x ^ ", " ^ string_of_quad_op m ^ ", -"
  | Q_ret -> "ret, -, -, -"


let icode = ref []

let addNewQuad quad =
  !icode <- quad :: !icode;
  ()

let rmLastQuad () =
  match !icode with
  | h::t ->
    !icode <- t;
    ()
  | _ ->
    error "intermediate code is empty!";
    raise Exit

let printIntermediateCode () =
  let pr quad =
    printf "%s\n" (string_of_quad quad)
  in
    printf "\n\nIntermediate code:\n";
    List.iter pr !icode;
    printf "\n"

(* cast quad operand to constant value *)
let const_of_quad = function
    | Q_int q -> CONST_int q
    | Q_real q -> CONST_REAL q
    | Q_bool q -> CONST_bool q
    | Q_char q -> CONST_char q
    | _ -> CONST_none

(* cast constant value to quad operand *)
let quad_of_const = function
    | CONST_int c -> Q_int c
    | CONST_REAL c -> Q_real c
    | CONST_bool c -> Q_bool c 
    | CONST_char c -> Q_char c
    | _ -> Q_none

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

(* Semantic Value of expr *)
type semv_expr = {
    e_place : quad_op_t;
    e_typ : Types.typ
  }

(* Used for errors *)
let esv_err = {
    e_place = Q_none;
    e_typ = TYPE_none; (* Maybe not needed, since place can tell *)
  }

  (*
  type sem_quad_t = {
    place : quad_op_t;
    typ : Types.typ;
    mutable q_next : int list;
    mutable q_true : int list;
    mutable q_false : int list
    }
  *)

(* Semantic Value of parameters *)
type semv_par = {
    p_name : Identifier.id;
    p_typ : Types.typ;
    p_mode : Symbol.pass_mode
  }

(* semantically check binary expression operands and find return type *)
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

(* semantically check unary expression operand and find return type *)
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

(* apply the binary operator to constant operands *)
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
let sq_binop a op b =
    (* TODO: generate actual quads *)
    (* TODO: division by zero check *)
    let typ = what_bin_type a op b in
    match typ with
    | TYPE_none ->
      begin
        error "Binary Operator %s Error: Operands mismatch" op;
        (* print_binop_type_error op a.e_typ b.e_typ *)
        esv_err
      end
    | _ ->
      let c1 = const_of_quad a.e_place in
      let c2 = const_of_quad b.e_place in
      match c1,c2 with
      | CONST_none, _
      | _, CONST_none ->
         (* result isn't a const *)
         (* make new temporary for result *)
         let e = newTemporary typ in
         let q = Q_op (op, a.e_place, b.e_place, Q_entry e) in
         let esv = {
           e_place = (Q_entry e);
           e_typ = typ
         } in
         addNewQuad q;
         esv
      | _ ->
         (* result is a const *)
         let plc = const_binop (op, typ, c1, c2) in
         let esv = {
           e_place = plc;
           e_typ = typ
         } in esv

(* Semantic-Quad actions for unary operators *)
let sq_unop op a =
    (* TODO: generate actual quads *)
    let typ = what_un_type op a in
    match typ with
    | TYPE_none ->
      begin
        error "Unary Operator %s Error" op;
        esv_err
      end
    | _ ->
      (* make new temporary for result *)
      let e = newTemporary typ in
      let esv = {
        e_place = (Q_entry e);
        e_typ = typ
      } in esv

(* Semantic-Quad actions for constant definiton *)
let sq_cdef n t v =
    try
      let e = lookupEntry (id_make n) LOOKUP_CURRENT_SCOPE false in
        begin
        (* if found, name is already taken *)
        error "Const name %s is already taken" n;
        ignore(e);
        ()
        end
    with Not_found ->
      (* if not found, check types *)
      if equalType t v.e_typ then
        (* if match, find the const value *)
        (* constant value must be known at compile-time *)
        let cv = const_of_quad v.e_place in
        match cv with
        | CONST_none ->
          begin
          (* not really a const *)
          error "%s is not really a const!" n;
          ()
          end
        | _ ->
          begin
          (* register the new Constant *)
          ignore (newConstant (id_make n) t cv false);
          ()
          end
      else
        begin
        (* const def type mismatch *)
        error "constant definition type mismatch";
        ()
        end

(* Semantic-Quad actions for lvalue *)
let sq_lvalue name idxs =
  (* a function that generates quads for array indexing *)
  let rec mktemp plc typ indexes =
    match typ, indexes with
    | TYPE_array(et, sz),h::t ->
       begin
       let newt = newTemporary et in
       let q = Q_array (plc, h.e_place, Q_entry newt) in
       addNewQuad q;
       mktemp (Q_deref newt) et t
       end
    | _, [] ->
       let esv = {
         e_place = plc;
         e_typ = typ
       } in esv
    | _ ->
       error "lvalue %s error!" name;
       esv_err
  in
  try
    (* Lookup the Symbol Table, do not handle the not_found case *)
    let e = lookupEntry (id_make name) LOOKUP_CURRENT_SCOPE false in
    begin
    (* check if entry is variable or parameter *)
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
           esv_err;
        | _ ->
           mktemp qv inf.constant_type idxs
        end
    | _ ->
        error "lvalue %s is not a variable or a parameter or a constant in the current scope!" name;
        esv_err
    end
  with Not_found ->
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

(* Semantic-Quads action for variable definition *)
let sq_vardef typ (name, dims, init) =
    try
      let e = lookupEntry (id_make name) LOOKUP_CURRENT_SCOPE false in
        begin
        (* if found, name is already taken *)
        error "Var name %s is already taken" name;
        ignore(e);
        ()
        end
    with Not_found ->
      (* if name not already taken *)
      match init.e_place with
      | Q_none ->
        (* no initialization or array *)
        begin
        let rec ft = function
        | (Q_int h)::t -> TYPE_array ( (ft t), h )
        | [] -> typ
        | _ -> error "Array dimension isn't int const"; TYPE_none
        in
        ignore( newVariable (id_make name) (ft dims) true )
        end
      | _ ->
        (* initialization is present *)
        (* check types *)
        if not (equalType typ init.e_typ) then
          (* var def type mismatch *)
          error "variable definition and initialization type mismatch"
        else
          (* if match, register the new Variable *)
          let n = newVariable (id_make name) typ true in
          let q = Q_assign ( (Q_entry n), init.e_place ) in
          addNewQuad q

(* Semantic-Quads action for routine header *)
let sq_rout_head name typ pars =
  (* register a new function or
   * find the forwarded function *)
  let e = newFunction (id_make name) true in
    begin
    openScope ();
    (* add parameters *)
    let paramadd (t,(n,m,dims)) =
      let rec ft = function
        | (Q_int d)::ds -> TYPE_array (ft ds, d)
        | [] -> t
        | _ -> TYPE_none
      in
        ignore (newParameter (id_make n) (ft dims) m e true)
    in
      begin
      (* List.rev $4; *)
      let q = Q_unit Q_entry e
      in addNewQuad q;
      List.iter paramadd pars;
      endFunctionHeader e typ;
      e
      end
    end

(* Semantic-Quads action for routine call *)
let sq_rout_call name pars =
  (* not found is handled *)
  let e = lookupEntry (id_make name) LOOKUP_ALL_SCOPES true
  in
    match e.entry_info with
    (* e should be function entry *)
    | ENTRY_function inf ->
        begin
        (* should be PARDEF_COMPLETE *)
        match inf.function_pstatus with
        | PARDEF_COMPLETE ->
           begin
           (* a function that matches array actual parameters with formal parameters *)
           let dimmatch formal actual =
           match formal, actual with
           | TYPE_array(et1,sz1), TYPE_array(et2, sz2) -> if (sz1 = 0 || sz1 = sz2) then equalArrayType et1 et2 else false
           | _ -> true
           in 
           (* function that matches formal parameters with actual parameters *)
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
                   begin
                   (* generate quads *)
                   let q = Q_par (ah.e_place, Q_pass_mode quad_t_of_pass_mode inf.parameter_mode) in
                   addNewQuad q;
                   parmatch (ft,at)
                   end
              | _ ->
                 error "what the hell? this ain't no parameter!";
                 false
              end
           | ([], []) -> true
           | ([], _)
           | (_, []) ->
              error "too many or not enough arguments";
              false
           in
           (* do the matching *)
           if (parmatch (inf.function_paramlist, pars)) then
             (* matching done successfully! *)
             begin
             match inf.function_result with
             | TYPE_proc ->
               (* generate call quad *)
               let q = Q_call Q_entry e in
               addNewQuad q;
               let esv = {
                  e_place = Q_none;
                  e_typ = TYPE_proc
               } in esv
             | _ ->
                let t = inf.function_result in
                let etemp = newTemporary t in
                let q = Q_par (Q_entry etemp, Q_pass_mode RET) in
                begin
                addNewQuad q;
                let q = Q_call Q_entry e in
                addNewQuad q;
                let esv = {
                  e_place = (Q_entry etemp);
                  e_typ = t
                } in esv
                end
             end
           else
             (* some error occured! *)
             esv_err
           end
        (* maybe should never reach *)
        | _ ->
           begin
           error "function params are not checked?";
           esv_err
           end
        end
    (* should never reach *)
    | _ ->
        begin
        error "Cannot call %s, not a function" name;
        esv_err
        end
