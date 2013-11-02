/*
(*
 * Pazcal Parser - Parser.mly
 *
 * compile with : $ ocamlyacc Parser.mly
 *
 *)
 */

%{
  (*/* Header - Ocaml code */*)
  open Printf
  open Lexing

  open Error
  open Identifier
  open Symbol
  open Symbtest
  open Types

  (* Called by the parser function on error *)
  let parse_error s = 
    printf "\n\t%s\n\n" s;
    flush stdout

  (* Quad operand datatype *)
  type quad_op_t = Q_None                           (* Error Handling *)
                 | Q_int of int                     (* Direct Integers *)
                 | Q_char of char                   (* Direct Characters *)
                 | Q_string of string               (* Direct String Literals *)
                 | Q_real of float                  (* Direct Reals *)
                 | Q_bool of bool                   (* Needed ??? *)
                 | Q_entry of Symbol.entry          (* Symbol Table entry i.e. name, temp *)
                 | Q_funct_res                      (* Function result: $$ *)
                 | Q_deref                          (* Dereference: [x] *)
                 | Q_addr                           (* Address: {x} *)
                 | Q_label                          (* label *)
                 | Q_pass_mode of quad_pass_mode    (* Pass mode: V, R, RET *)
                 | Q_empty                          (* Empty : - *)
                 | Q_backpatch                      (* Backpatch : * *)

  and quad_pass_mode = V | R | RET

  (* Semantic Value of expr *)
  type semv_expr = {
    place : quad_op_t;
    typ : Types.typ
  }

  (* Used for errors *)
  let sv_err = {
    place = Q_None;
    typ = TYPE_none; (* Maybe not needed, since place can tell *)
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


  (* Simple Function to get Expression Position *)
  let get_binop_pos () =
    (rhs_start_pos 1, rhs_start_pos 3)

  (* convert Type to string *)
  let rec string_of_typ typ =
    match typ with
    |TYPE_none -> "<undefined>"
    |TYPE_int -> "int"
    |TYPE_bool -> "bool"
    |TYPE_char -> "char"
    |TYPE_REAL -> "REAL"
    |TYPE_array(et,sz) when sz > 0 -> String.concat "" [(string_of_typ et);("[");(string_of_int sz);("]")]
    |TYPE_array(et,sz) -> String.concat "" [(string_of_typ et);("[]")]

  (* print an error message *)
  let print_type_error op_name t1 t2 exp_t sp ep =
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

  (* semantically check operands and find return type *)
  let what_bin_type a op b =
    match op with
    | "+"
    | "-"
    | "*"
    | "/" ->
      begin
      match (a.typ, b.typ) with
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
      match (a.typ, b.typ) with
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
      match (a.typ, b.typ) with
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
      match (a.typ, b.typ) with
      | (TYPE_bool, TYPE_bool) ->
        TYPE_bool
      | (_,_) ->
        TYPE_none
      end
    | _ ->
      TYPE_none

  (* semantically check operand and find return type *)
  let what_un_type op a =
    match op with
    | "+"
    | "-" ->
      begin
      match a.typ with
      | TYPE_int -> TYPE_int
      | TYPE_char -> TYPE_char
      | TYPE_REAL -> TYPE_REAL
      | _ -> TYPE_none
      end
    | "!" ->
      begin
      match a.typ with
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
        sv_err
      end
    | _ ->
      (* make new temporary for result *)
      let e = newTemporary typ in
      let sv = {
        place = (Q_entry e);
        typ = typ
      } in sv

  (* Semantic-Quad actions for unary operators *)
  let sq_unop op a =
    (* TODO: generate actual quads *)
    let typ = what_un_type op a in
    match typ with
    | TYPE_none ->
      begin
        error "Unary Operator %s Error" op;
        sv_err
      end
    | _ ->
      (* make new temporary for result *)
      let e = newTemporary typ in
      let sv = {
        place = (Q_entry e);
        typ = typ
      } in sv

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
      if equalType t v.typ then
        (* if match, find the const value *)
        (* constant value must be known at compile-time *)
        let cval = function
          | Q_int (a) -> CONST_int a
          | Q_char (a) -> CONST_char a
          | Q_string (a) -> CONST_string a
          | Q_real (a) -> CONST_REAL a
          | _ -> CONST_none
        in
          let cv = cval v.place in
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
    (* Lookup the Symbol Table *)
    let e = lookupEntry (id_make a) LOOKUP_CURRENT_SCOPE true in
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
        match c.place with
        | Q_None ->
          begin
          (* no initialization *)
          let e = newVariable (id_make a) t true
          in ignore(e)
          end
        | _ ->
          (* check types *)
          if equalType t c.typ then
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


  (* first steps *)
  let prologue () =
    printf "Start!\n";
    initSymbolTable 256;
    openScope ()

  (* last steps *)
  let epilogue () =
    printf "End!\n";
    printSymbolTable ()

%}


/* Ocamlyacc Declarations */

%token T_bool T_break T_case T_char T_const
%token T_continue T_default T_do T_DOWNTO T_else T_false
%token T_FOR T_FORM T_FUNC T_if T_int T_MOD
%token T_NEXT T_PROC T_PROGRAM T_REAL
%token T_return T_STEP T_switch T_TO T_true T_while
%token T_WRITE T_WRITELN T_WRITESP T_WRITESPLN
%token <string> T_id
%token <int> T_int_const
%token <float> T_float_const
%token <char> T_char_const
%token <string> T_string_literal
%token T_amp T_sem_col T_dot T_lparen T_rparen T_col
%token T_comma T_lbrack T_rbrack T_lbrace T_rbrace
%token T_plus T_minus T_lg_not T_not
%token T_mul T_div T_mod T_MOD
%token T_gr T_ls T_lseq T_greq
%token T_eq T_neq
%token T_lg_and T_and
%token T_lg_or T_or
%token T_assign T_plus_assign T_minus_assign T_mul_assign T_div_assign T_mod_assign
%token T_plus_plus 
%token T_minus_minus
%token T_EOF

%nonassoc NOELSE      /* pseudo-token. it gives T_ELSE higher precedence */
%nonassoc T_else

%left T_lg_or T_or
%left T_lg_and T_and
%left T_eq T_neq
%left T_gr T_ls T_greq T_lseq
%left T_plus T_minus
%left T_mul T_div T_mod T_MOD
%left UNARY

%start pazprog
%type <unit> pazprog
/* %type <sem_quad_t> expr */
%type <semv_expr> expr
%type <Types.typ> paztype


/*(* Grammar follows *)*/

/*
(*
 * Because we're generating an LALR(1) parser,
 * left-recursive grammar rules are good!
 *)
 */

/*
(*
 * TODO: Do not put any identation for semantic brackets before we know it runs smoothly
 * TODO: Location Tracking
 *
 *)
 */

%%

pazprog : /* empty */ { }
        | dummy_non_terminal declaration_list T_EOF { epilogue ()  }
		;

dummy_non_terminal : /* empty, used only for semantic actions */ { prologue () }

declaration_list : declaration { }
                 | declaration_list declaration { }
                 ;

declaration : const_def { }
            | var_def { }
            | routine { }
            | program { }
            ;

/*(* batzilo 30/10 *)*/
const_def :	T_const paztype T_id T_assign const_expr T_sem_col {
                (* semantic-quad actions for constant definition. name, type, value *)
                sq_cdef $3 $2 $5
                }
          | T_const paztype T_id T_assign const_expr const_def2 T_sem_col {
                (* For every tuple in list, register a new Constant *)
                let
                  reg_all a (b,c) = sq_cdef b a c 
                in
                  begin
                  (* register the first *)
                  sq_cdef $3 $2 $5;
                  (* register the rest *)
                  List.iter (reg_all $2) $6
                  end
                }
          ;

const_def2 : T_comma T_id T_assign const_expr {
                (* Return a tuple (name, value) *)
                ($2, $4)::[]
                }
           | const_def2 T_comma T_id T_assign const_expr {
                (* Return a list of tuples *)
                ($3, $5) :: $1
                }
           ;

/*(* batzilo 2/11 *)*/
var_def : paztype var_init T_sem_col {
                sq_vardef $1 $2
                }
        | paztype var_init var_def2 T_sem_col {
                sq_vardef $1 $2;
                List.iter (sq_vardef $1) $3
                }
		;

var_def2 : T_comma var_init { $2::[] }
         | var_def2 T_comma var_init { $1 @ $3::[] }
         ;

var_init : simple_var_init { $1 }
         | matrix_var_init { $1 }
         ;

simple_var_init : T_id { ($1, [], sv_err) }
                | T_id T_assign expr { ($1, [], $3) }
                ;

matrix_var_init : T_id T_lbrack const_expr T_rbrack { ($1, $3::[], sv_err) }
                | T_id T_lbrack const_expr T_rbrack matrix_var_init2 { ($1, $3::$5, sv_err) }
                ;

matrix_var_init2 : T_lbrack const_expr T_rbrack { $2::[] }
                 | matrix_var_init2 T_lbrack const_expr T_rbrack { $1 @ $3::[] }
                 ;

routine_header : T_PROC T_id T_lparen T_rparen { }
               | T_PROC T_id T_lparen paztype formal routine_header2 T_rparen { }
               | T_FUNC paztype T_id T_lparen T_rparen { }
               | T_FUNC paztype T_id T_lparen paztype formal routine_header2 T_rparen { }
               ;

routine_header2 : T_comma paztype formal { }
                | routine_header2 T_comma paztype formal { }
                ;

formal : T_id { }
       | T_amp T_id { }
       | T_id T_lbrack T_rbrack { }
       | T_id T_lbrack T_rbrack formal2 { }
       | T_id T_lbrack const_expr T_rbrack { }
       | T_id T_lbrack const_expr T_rbrack formal2 { }
       ;

formal2 : T_lbrack const_expr T_rbrack { }
        | formal2 T_lbrack const_expr T_rbrack { }
        ;

routine : routine_header T_sem_col { }
        | routine_header block { }
        ;

program_header : T_PROGRAM T_id T_lparen T_rparen { }
               ;

program : program_header block { }
        ;

paztype : T_int { TYPE_int }
        | T_bool { TYPE_bool }
        | T_char { TYPE_char }
        | T_REAL { TYPE_REAL }
        ;

const_expr : expr { $1 }
           ;

/*
(* edited 17/10 - added semantic checks *)
(* edited 30/10 - more semantic-quad actions *)
(* TODO: separate binop and unop ? *)
*/
expr : T_int_const {
            let sv = {
              place = (Q_int $1);
              typ = TYPE_int;
            } in sv
        }
     | T_float_const {
            let sv = {
              place = (Q_real $1);
              typ = TYPE_REAL;
            } in sv
        }
     | T_char_const {
            let sv = {
              place = (Q_char $1);
              typ = TYPE_char;
            } in sv
        }
     | T_string_literal {
            let sv = {
              place = (Q_string $1);
              typ = TYPE_array(TYPE_char, String.length $1);
            } in sv
        }
     | T_true {
            let sv = {
              place = (Q_bool true);
              typ = TYPE_bool;
            } in sv
        }
     | T_false {
            let sv = {
              place = (Q_bool false);
              typ = TYPE_bool;
            } in sv
        }
     | T_lparen expr T_rparen       { $2 }
     | l_value                      { $1 }
     | call                         { sv_err (* TODO: complete when done with functions *) }
     | T_plus expr %prec UNARY      { sq_unop "+" $2 }
     | T_minus expr %prec UNARY     { sq_unop "-" $2 }
     | T_lg_not expr %prec UNARY    { sq_unop "!" $2 }
     | T_not expr %prec UNARY       { sq_unop "!" $2 }
     | expr T_plus expr             { sq_binop $1 "+" $3 }
     | expr T_minus expr            { sq_binop $1 "-" $3 }
     | expr T_mul expr              { sq_binop $1 "*" $3 }
     | expr T_div expr              { sq_binop $1 "/" $3 }
     | expr T_mod expr              { sq_binop $1 "%" $3 }
     | expr T_MOD expr              { sq_binop $1 "%" $3 }
     | expr T_eq expr               { sq_binop $1 "==" $3 }
     | expr T_neq expr              { sq_binop $1 "!=" $3 }
     | expr T_ls expr               { sq_binop $1 "<" $3 }
     | expr T_gr expr               { sq_binop $1 ">" $3 }
     | expr T_lseq expr             { sq_binop $1 "<=" $3 }
     | expr T_greq expr             { sq_binop $1 ">=" $3 }
     | expr T_lg_and expr           { sq_binop $1 "&&" $3 }
     | expr T_and expr              { sq_binop $1 "&&" $3 }
     | expr T_lg_or expr            { sq_binop $1 "||" $3 }
     | expr T_or expr               { sq_binop $1 "||" $3 }
     ;

/*(* batzilo 30/10 *)*/
l_value : T_id {
            sq_lvalue $1
            }
        | T_id l_value2 {
            (* add params *)
            sq_lvalue $1
            }
		;

l_value2 : T_lbrack expr T_rbrack { $2::[] }
         | l_value2 T_lbrack expr T_rbrack { $3 :: $1 }
         ;


call : T_id T_lparen T_rparen { }
     | T_id T_lparen call2 T_rparen { }
     ;

call2 : expr { }
      | call2 T_comma expr { }
      ;

block : T_lbrace T_rbrace { }
      | T_lbrace block2 T_rbrace { }
      /* error recovery */
      | T_lbrace error T_rbrace { }
      ;

block2 : local_def { }
       | stmt { }
       | block2 local_def { }
       | block2 stmt { }
       ;

local_def :	const_def { }
          | var_def { }
          ;

stmt : T_sem_col { }
     | l_value assign expr T_sem_col { }
     | l_value T_plus_plus T_sem_col { }
     | l_value T_minus_minus T_sem_col { }
     | call T_sem_col { }
     | T_if T_lparen expr T_rparen stmt	%prec NOELSE { }
     | T_if T_lparen expr T_rparen stmt T_else stmt { }
     | T_while T_lparen expr T_rparen stmt { }
     | T_FOR T_lparen T_id T_comma range T_rparen stmt { }
     | T_do stmt T_while T_lparen expr T_rparen T_sem_col { }
     /* switch ? */
     | T_break T_sem_col { }
     | T_continue T_sem_col { }
     | T_return T_sem_col { }
     | T_return expr T_sem_col { }
     | write T_lparen T_rparen T_sem_col { }
     | write T_lparen stmt2 T_rparen T_sem_col { }
     /* error recovery */
     | error T_sem_col { }
     ;

stmt2 : format { }
      | stmt2 T_comma format { }
      ;

assign : T_assign { }
       | T_plus_assign { }
       | T_minus_assign { }
       | T_mul_assign { }
       | T_div_assign { }
       | T_mod_assign { }
       ;

range : expr T_TO expr { }
      | expr T_TO expr T_STEP expr { }
      | expr T_DOWNTO expr { }
      | expr T_DOWNTO expr T_STEP expr { }
      ;

/* clause */

write : T_WRITE { }
      | T_WRITELN { }
      | T_WRITESP { }
      | T_WRITESPLN { }
      ;

format : expr { }
       | T_FORM T_lparen expr T_comma expr T_rparen	{ }
       | T_FORM T_lparen expr T_comma expr T_comma expr T_rparen { }
       ;

%%
