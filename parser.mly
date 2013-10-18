/*
 * Pazcal Parser - parser.mly
 *
 * compile with : $ ocamlyacc parser.mly
 */

%{
  (*/* Header - Ocaml code */*)
  open Printf
  open Lexing

  open Error
  open Identifier
  open Symbol
  open Types

  (* Called by the parser function on error *)
  let parse_error s = 
    printf "\n\t%s\n\n" s;
    flush stdout

  type quad_op_t = Q_none
                 | Q_int of int
                 | Q_char of char
                 | Q_string of string
                 | Q_real of float
                 | Q_entry of Symbol.entry
                 (* TODO: Quad_valof *)
    
  and sem_quad_t = {
    place : quad_op_t;
    typ : Types.typ;
    mutable q_next : int list;
    mutable q_true : int list;
    mutable q_false : int list
    }

  let sq_err = {
        place = Q_none;
        typ = TYPE_none;
        q_next = [];
        q_true = [];
        q_false = [];
    }

    (* Simple Function to get Expression Position *)
    let get_binop_pos () =
        (rhs_start_pos 1, rhs_start_pos 3)

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
     
    (*
    let print_div_zero_error =
    *)
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
%type <sem_quad_t> expr
/* %type <paztype> Types.typ */


/* Grammar follows */

/* Since we're generating an LALR(1) parser,
 * left-recursive grammar rules are good! */

/* TODO:
 *
 * Do not put any identation for semantic brackets before we know it runs smoothly
 * Location Tracking
 *
 */

%%

pazprog : /* empty */ { }
        | dummy_non_terminal declaration_list { }
		;

dummy_non_terminal : /* empty, used only for semantic actions */ { initSymbolTable 256 }

declaration_list : declaration { }
                 | declaration_list declaration { }
                 ;

declaration : const_def { }
            | var_def { }
            | routine { }
            | program { }
            ;

const_def :	T_const paztype T_id T_assign const_expr T_sem_col { }
          | T_const paztype T_id T_assign const_expr const_def2 T_sem_col { }
          ;

const_def2 : T_comma T_id T_assign const_expr { }
           | const_def2 T_comma T_id T_assign const_expr { }
           ;

var_def : paztype var_init T_sem_col { (*
                let (id, sq, dims) = $2 in
                    (*
                    begin
                    *)
                    ignore(newVariable (id_make id) $1 true)
                    (* TODO: initialization code
                    match sq with
                        | sq_err -> ignore(newVariable (id_make id) $1 true)
                        | _ -> 
                            begin
                            (* initialization quad *)
                            end;
                    *)
                    (*
                    end
                    *)
          *)  }
        | paztype var_init var_def2 T_sem_col { }
		;

var_def2 : T_comma var_init { }
         | var_def2 T_comma var_init { }
         ;

var_init : simple_var_init { $1 }
         | matrix_var_init { $1 }
         ;

simple_var_init : T_id { ($1, sq_err, []) }
                | T_id T_assign expr { ($1, $3, []) }
                ;

matrix_var_init : T_id T_lbrack const_expr T_rbrack { ($1, sq_err, $3) }
                | T_id T_lbrack const_expr T_rbrack matrix_var_init2 { }
                ;

matrix_var_init2 : T_lbrack const_expr T_rbrack { }
                 | matrix_var_init2 T_lbrack const_expr T_rbrack {  }
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

const_expr : expr { }
           ;

/*
(* edited 17/10 - added semantic checks *)
(* TODO: separate binop and unop ? *)
*/
expr : T_int_const {
            let sq = {
                place = $1;
                typ = TYPE_int;
                q_next = [];
                q_true = [];
                q_false = [];
            } in sq
        }
     | T_float_const {
            let sq = {
                place = $1;
                typ = TYPE_REAL;
                q_next = [];
                q_true = [];
                q_false = [];
            } in sq
        }
     | T_char_const {
            let sq = {
                place = $1;
                typ = TYPE_char;
                q_next = [];
                q_true = [];
                q_false = [];
            } in sq
        }
     | T_string_literal {
            let sq = {
                place = $1;
                typ = TYPE_array(TYPE_char, String.length $1);
                q_next = [];
                q_true = [];
                q_false = [];
            } in sq
        }
     | T_true {
            let sq = {
                place = "true";
                typ = TYPE_bool;
                q_next = [];
                q_true = [];
                q_false = [];
            } in sq
        }
     | T_false {
            let sq = {
                place = "false";
                typ = TYPE_bool;
                q_next = [];
                q_true = [];
                q_false = [];
            } in sq
        }
     | T_lparen expr T_rparen { $2 }
     | l_value { $1 }
     | call { $1 }
     | T_plus expr %prec UNARY { 
            if $2.typ != TYPE_int then
                begin
                print_type_error "+" $2 TYPE_int (rhs_start_pos 2);
                sq_err
                end
            else
                $2
        }
     | T_minus expr %prec UNARY {
            if $2.typ != TYPE_int then
                begin
                print_type_error "+" $2 TYPE_int (rhs_start_pos 2);
                sq_err
                end
            else
                (* stub *)
                let sq = {
                    place = newTemporary TYPE_int;
                    typ = TYPE_int;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | T_lg_not expr %prec UNARY {
            if $2.typ != TYPE_bool then
                begin
                print_type_error "+" $2 TYPE_bool (rhs_start_pos 2);
                sq_err
                end
            else
                (* stub *)
                let sq = {
                    place = newTemporary TYPE_bool;
                    typ = TYPE_bool;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | T_not expr %prec UNARY {
            if $2.typ != TYPE_bool then
                begin
                print_type_error "+" $2 TYPE_bool (rhs_start_pos 2);
                sq_err
                end
            else
                (* stub *)
                let sq = {
                    place = newTemporary TYPE_bool;
                    typ = TYPE_bool;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | expr T_plus expr {
            if $1.typ != TYPE_int || $3.typ != TYPE_int then
                begin
                print_type_error "+" $1 $3 TYPE_int (get_binop_pos ());
                sq_err
                end
            else
                let pl = match ($1.place, $3.place) with
                    | (q_int as x1, q_int as x2) -> x1 + x2
                    | (_,_) -> newTemporary TYPE_int
                in
                let sq = {
                    place = pl;
                    typ = TYPE_int;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | expr T_minus expr {
            if $1.typ != TYPE_int || $3.typ != TYPE_int then
                begin
                print_type_error "-" $1 $3 TYPE_int (get_binop_pos ());
                sq_err
                end
            else
                let pl = match ($1.place, $3.place) with
                    | (q_int as x1, q_int as x2) -> x1 - x2
                    | _ -> newTemporary TYPE_int
                in
                let sq = {
                    place = pl;
                    typ = TYPE_int;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | expr T_mul expr {
            if $1.typ != TYPE_int || $3.typ != TYPE_int then
                begin
                print_type_error "*" $1 $3 TYPE_int (get_binop_pos());
                sq_err
                end
            else
                let pl = match ($1.place, $3.place) with
                    | (q_int as x1, q_int as x2) -> x1 * x2
                    | _ -> newTemporary TYPE_int
                in
                let sq = {
                    place = pl;
                    typ = TYPE_int;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | expr T_div expr {
            if $1.typ != TYPE_int || $3.typ != TYPE_int then
                begin
                print_type_error "/" $1 $3 TYPE_int (get_binop_pos());
                sq_err
                end
            (*
            else if $3.place == 0 then
                print_div_zero_error;
                sq_err
            *)
            else
                let pl = match ($1.place, $3.place) with
                    | (q_int as x1, q_int as x2) -> x1 / x2
                    | _ -> newTemporary TYPE_int
                in
                let sq = {
                    place = pl;
                    typ = TYPE_int;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | expr T_mod expr {
            if $1.typ != TYPE_int || $3.typ != TYPE_int then
                begin
                print_type_error "%" $1 $3 TYPE_int (get_binop_pos());
                sq_err
                end
            (*
            else if $3.place == 0 then
                print_div_zero_error;
                sq_err
            *)
            else
                let pl = match ($1.place, $3.place) with
                    | (q_int as x1, q_int as x2) -> x1 % x2
                    | _ -> newTemporary TYPE_int
                in
                let sq = {
                    place = pl;
                    typ = TYPE_int;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | expr T_MOD expr {
            if $1.typ != TYPE_int || $3.typ != TYPE_int then
                begin
                print_type_error "%" $1 $3 TYPE_int (get_binop_pos());
                sq_err
                end
            (*
            else if $3.place == 0 then
                print_div_zero_error;
                sq_err
            *)
            else
                let pl = match ($1.place, $3.place) with
                    | (q_int as x1, q_int as x2) -> x1 % x2
                    | _ -> newTemporary TYPE_int
                in
                let sq = {
                    place = pl;
                    typ = TYPE_int;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | expr T_eq expr {
            if $1.typ != TYPE_int || $3.typ != TYPE_int then
                begin
                print_type_error "=" $1 $3 $1.typ (get_binop_pos());
                sq_err
                end
            else
                let pl = match ($1.place, $3.place) with
                    | (q_int as x1, q_int as x2) -> x1 = x2
                    | _ -> newTemporary TYPE_bool
                in
                let sq = {
                    place = pl;
                    typ = TYPE_bool;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | expr T_neq expr {
            if $1.typ != TYPE_int || $3.typ != TYPE_int then
                begin
                print_type_error "!=" $1 $3 $1.typ (get_binop_pos());
                sq_err
                end
            else
                let pl = match ($1.place, $3.place) with
                    | (q_int as x1, q_int as x2) -> x1 != x2
                    | _ -> newTemporary TYPE_bool
                in
                let sq = {
                    place = pl;
                    typ = TYPE_bool;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | expr T_ls expr {
            if $1.typ != TYPE_int || $3.typ != TYPE_int then
                begin
                print_type_error "<" $1 $3 $1.typ (get_binop_pos());
                sq_err
                end
            else
                let pl = match ($1.place, $3.place) with
                    | (q_int as x1, q_int as x2) -> x1 < x2
                    | _ -> newTemporary TYPE_bool
                in
                let sq = {
                    place = pl;
                    typ = TYPE_bool;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | expr T_gr expr {
            if $1.typ != TYPE_int || $3.typ != TYPE_int then
                begin
                print_type_error ">" $1 $3 $1.typ (get_binop_pos());
                sq_err
                end
            else
                let pl = match ($1.place, $3.place) with
                    | (q_int as x1, q_int as x2) -> x1 > x2
                    | _ -> newTemporary TYPE_bool
                in
                let sq = {
                    place = pl;
                    typ = TYPE_bool;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | expr T_lseq expr {
            if $1.typ != TYPE_int || $3.typ != TYPE_int then
                begin
                print_type_error "<=" $1 $3 $1.typ (get_binop_pos());
                sq_err
                end
            else
                let pl = match ($1.place, $3.place) with
                    | (q_int as x1, q_int as x2) -> x1 <= x2
                    | _ -> newTemporary TYPE_bool
                in
                let sq = {
                    place = pl;
                    typ = TYPE_bool;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | expr T_greq expr {
            if $1.typ != TYPE_int || $3.typ != TYPE_int then
                begin
                print_type_error ">=" $1 $3 $1.typ (get_binop_pos());
                sq_err
                end
            else
                let pl = match ($1.place, $3.place) with
                    | (q_int as x1, q_int as x2) -> x1 >= x2
                    | _ -> newTemporary TYPE_bool
                in
                let sq = {
                    place = pl;
                    typ = TYPE_bool;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | expr T_lg_and expr {
            if $1.typ != TYPE_bool || $3.typ != TYPE_bool then
                begin
                print_type_error "&&" $1 $3 $1.typ (get_binop_pos());
                sq_err
                end
            else
                let pl = match ($1.place, $3.place) with
                    | (q_bool as x1, q_bool as x2) -> x1 && x2
                    | _ -> newTemporary TYPE_bool
                in
                let sq = {
                    place = pl;
                    typ = TYPE_bool;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | expr T_and expr {
            if $1.typ != TYPE_bool || $3.typ != TYPE_bool then
                begin
                print_type_error "&&" $1 $3 $1.typ (get_binop_pos());
                sq_err
                end
            else
                let pl = match ($1.place, $3.place) with
                    | (q_bool as x1, q_bool as x2) -> x1 && x2
                    | _ -> newTemporary TYPE_bool
                in
                let sq = {
                    place = pl;
                    typ = TYPE_bool;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | expr T_lg_or expr {
            if $1.typ != TYPE_bool || $3.typ != TYPE_bool then
                begin
                print_type_error "||" $1 $3 $1.typ (get_binop_pos());
                sq_err
                end
            else
                let pl = match ($1.place, $3.place) with
                    | (q_bool as x1, q_bool as x2) -> x1 || x2
                    | _ -> newTemporary TYPE_bool
                in
                let sq = {
                    place = pl;
                    typ = TYPE_bool;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     | expr T_or expr {
            if $1.typ != TYPE_bool || $3.typ != TYPE_bool then
                begin
                print_type_error "||" $1 $3 $1.typ (get_binop_pos());
                sq_err
                end
            else
                let pl = match ($1.place, $3.place) with
                    | (q_bool as x1, q_bool as x2) -> x1 || x2
                    | _ -> newTemporary TYPE_bool
                in
                let sq = {
                    place = pl;
                    typ = TYPE_bool;
                    q_next = [];
                    q_true = [];
                    q_false = [];
                } in sq
        }
     ;

l_value : T_id { }
        | T_id l_value2 { }
		;

l_value2 : T_lbrack expr T_rbrack { }
         | l_value2 T_lbrack expr T_rbrack { }
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
