(* Quadruples *)

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
               | Q_deref                          (* Dereference: [x] *)
               | Q_addr                           (* Address: {x} *)
               | Q_label                          (* label *)
               | Q_pass_mode of quad_pass_mode    (* Pass mode: V, R, RET *)
               | Q_empty                          (* Empty : - *)
               | Q_backpatch                      (* Backpatch : * *)

and quad_pass_mode = V | R | RET

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

(* Semantic-Quad actions for binary operators *)
let sq_binop a op b =
    (* TODO: generate actual quads *)
    (* TODO: division by zero check *)
    let typ = what_bin_type a op b in
    match typ with
    | TYPE_none ->
      begin
        error "Binary Operator %s Error" op;
        esv_err
      end
    | _ ->
      (* make new temporary for result *)
      let e = newTemporary typ in
      let esv = {
        e_place = (Q_entry e);
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
(* TODO : Add params because array lvalue place should be a temporary after generating array,a,i,$1 *)
(* TODO : Use LOOKUP_ALL_SCOPES when looking for constants *)
let sq_lvalue a =
    (* Lookup the Symbol Table, not found is handled *)
    let e = lookupEntry (id_make a) LOOKUP_CURRENT_SCOPE true in
    begin
    (* check if entry is variable or parameter or constant *)
    match e.entry_info with
    | ENTRY_variable inf ->
        let esv = {
          e_place = (Q_entry e);
          e_typ = inf.variable_type
        } in esv
    | ENTRY_parameter inf ->
        let esv = {
          e_place = (Q_entry e);
          e_typ = inf.parameter_type
        } in esv
    | ENTRY_constant inf -> 
        (* TODO ? cast CONST_int to Q_int ? *)
        (* match inf.constant_value with *)
        let qv = quad_of_const inf.constant_value in
        let esv = {
          e_place = qv;
          e_typ = inf.constant_type
        } in esv
    | _ ->
        error "lvalue %s is not a variable or a parameter or a constant" a;
        esv_err
    end
    (*
    (* check if entry is variable or parameter or constant *)
    let etyp = function
      | ENTRY_variable (a) -> a.variable_type
      | ENTRY_parameter (a) -> a.parameter_type
      | ENTRY_constant (a) -> a.constant_type
      | _ -> TYPE_none
    in
      let lt = etyp e.entry_info in
      match lt with
      | TYPE_none ->
        begin
        error "lvalue %s not an variable or an parameter or a constant" a;
        sv_err
        end
      | _ ->
        begin
        let sv = {
          place = (Q_entry e);
          typ = lt;
        } in sv
        end
    *)

(* Semantic-Quads action for variable definition *)
let sq_vardef t foo =
    let reg (a,b,c) =
      try
        let e = lookupEntry (id_make a) LOOKUP_CURRENT_SCOPE false in
          begin
          (* if found, name is already taken *)
          error "Var name %s is already taken" a;
          ignore(e);
          ()
          end
      with Not_found ->
        (* if not found *)
        match c.e_place with
        | Q_none ->
          begin
          (* no initialization *)
          let e = newVariable (id_make a) t true
          in ignore(e)
          end
        | _ ->
          (* check types *)
          if equalType t c.e_typ then
            (* if match, register the new Variable *)
            let e = newVariable (id_make a) t true
            in ignore(e)
          else
            begin
            (* var def type mismatch *)
            error "variable definition type mismatch";
            ()
            end
    in
      reg foo

(* Semantic-Quads action for routine header *)
let sq_rout_head name typ pars =
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
      List.iter paramadd pars;
      endFunctionHeader e typ;
      e
      end
    end
