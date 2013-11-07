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

(* Semantic-Quad actions for binary operators *)
let sq_binop a op b =
    (* TODO: generate actual quads *)
    (* TODO: division by zero check *)
    let typ = what_bin_type a op b in
    match typ with
    | TYPE_none ->
      begin
        error "Binary Operator %s Error" op;
        (* print_binop_type_error op a.e_typ b.e_typ *)
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
let sq_lvalue name =
  try
    (* Lookup the Symbol Table *)
    let e = lookupEntry (id_make name) LOOKUP_CURRENT_SCOPE false in
    begin
    (* check if entry is variable or parameter *)
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
    | _ ->
        error "lvalue %s is not a variable or a parameter in the current scope!" name;
        esv_err
    end
  with Not_found ->
    let e = lookupEntry (id_make name) LOOKUP_ALL_SCOPES true in
    match e.entry_info with
    | ENTRY_constant inf -> 
      let qv = quad_of_const inf.constant_value in
      let esv = {
        e_place = qv;
        e_typ = inf.constant_type
      } in esv
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
        (* check types *)
        if not (equalType typ init.e_typ) then
          (* var def type mismatch *)
          error "variable definition type mismatch"
        else
          (* if match, register the new Variable *)
          ignore( newVariable (id_make name) typ true )

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
        match inf.function_pstatus with
        | PARDEF_COMPLETE ->
           begin
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
                   parmatch (ft,at)
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
                let esv = {
                   e_place = Q_none;
                   e_typ = TYPE_proc
                } in esv
             | _ ->
                begin
                let t = inf.function_result in
                let etemp = newTemporary t in
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
