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
  open SemQuad
  open Symbol
  open Symbtest
  open Types

  (* Called by the parser function on error *)
  let parse_error s = 
    printf "\n\t%s\n\n" s;
    flush stdout

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

  (* first steps *)
  let prologue () =
    printf "Start parsing!\n";
    initSymbolTable 256;
    openScope ()

  (* last steps *)
  let epilogue () =
    printf "End parsing!\n";
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

simple_var_init : T_id { ($1, [], esv_err) }
                | T_id T_assign expr { ($1, [], $3) }
                ;

matrix_var_init : T_id T_lbrack const_expr T_rbrack { ($1, $3::[], esv_err) }
                | T_id T_lbrack const_expr T_rbrack matrix_var_init2 { ($1, $3::$5, esv_err) }
                ;

matrix_var_init2 : T_lbrack const_expr T_rbrack { $2::[] }
                 | matrix_var_init2 T_lbrack const_expr T_rbrack { $1 @ $3::[] }
                 ;

/*(* batzilo 5/11 *)*/
routine_header : T_PROC T_id T_lparen T_rparen {
                        (* i.e PROC foo() *)
                        let e = newFunction (id_make $2) true in
                          begin
                          endFunctionHeader e TYPE_none;
                          e
                          end
                        }
               | T_PROC T_id T_lparen routine_header2 T_rparen {
                        (* i.e PROC foo( int a, char b, REAL c, bool d ) *)
                        let e = newFunction (id_make $2) true in
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
                            List.iter paramadd $4;
                            endFunctionHeader e TYPE_none;
                            e
                            end
                          end
                        }
               | T_FUNC paztype T_id T_lparen T_rparen {
                        (* i.e FUNC int foo() *)
                        let e = newFunction (id_make $3) true in
                          begin
                          endFunctionHeader e $2;
                          e
                          end
                        }
               | T_FUNC paztype T_id T_lparen routine_header2 T_rparen {
                        (* i.e FUNC int foo( int a, char b, REAL c, bool d ) *)
                        let e = newFunction (id_make $3) true in
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
                            (* List.rev $5; *)
                            List.iter paramadd $5;
                            endFunctionHeader e $2;
                            e
                            end
                          end
                        }
               ;

/*(* return a list of tuples (type, (name, mode, dims)) *)*/
routine_header2 : paztype formal { [ ($1, $2 ) ] }
                | routine_header2 T_comma paztype formal { $1 @ [ ($3, $4) ] }
                ;

/*(* return a triplet (name, pass_mode, dims) *)*/
formal : T_id {
              (*
              let sv = {
                name = id_make $1;
                typ = TYPE_none;
                pmode = PASS_BY_VALUE;
              } in sv
              *)
              ($1, PASS_BY_VALUE, [])
            }
       | T_amp T_id {
              (*
              let sv = {
                name = id_make $2;
                typ = TYPE_none;
                pmode = PASS_BY_REFERENCE;
              } in sv
              *)
              ($2, PASS_BY_REFERENCE, [])
            }
       | T_id T_lbrack T_rbrack {
              (*
              let sv = {
                name = id_make $1;
                typ = TYPE_array (TYPE_none, 0);
                pmode = PASS_BY_VALUE;
              } in sv
              *)
              ($1, PASS_BY_VALUE, [Q_int 0])
            } 
       | T_id T_lbrack T_rbrack formal2 {
              (*
              let mktyp l = function
                match l with
                | h::t -> TYPE_array (mktyp t, h)
                | [] -> TYPE_none
              in
              let sv = {
                name = id_make $1;
                typ = TYPE_array(mktyp $4, 0);
                pmode = PASS_BY_VALUE;
              } in sv
              *)
              ($1, PASS_BY_VALUE, Q_int 0 :: $4)
            }
       | T_id T_lbrack const_expr T_rbrack {
              (*
              if not equalType $3.typ TYPE_int then
                error "array dimension is not int const";
                (* what if error? *)
              else
              let sv = {
                name = id_make $1;
                typ = TYPE_array(TYPE_none, $3.place);
                pmode = PASS_BY_VALUE
              } in sv
              *)
              ($1, PASS_BY_VALUE, [$3.e_place])
            }
       | T_id T_lbrack const_expr T_rbrack formal2 {
              (*
              let mktyp l = function
                match l with
                | h::t -> TYPE_array (mktyp t, h)
                | [] -> TYPE_none
              in
              let sv = {
                name = id_make $1;
                typ = mktyp $4;
                pmode = PASS_BY_VALUE
              } in sv
              *)
              ($1, PASS_BY_VALUE, $3.e_place :: $5)
            }
       ;

/*(* return an Q_int list *)*/
formal2 : T_lbrack const_expr T_rbrack {
              (* the first *)
              match ($2.e_place, $2.e_typ) with
              | (Q_int v, TYPE_int) -> [Q_int v]
              | _ ->
                error "parameter array dimension is not an integer constant";
                (* TODO what if error? *)
                []
            }
        | formal2 T_lbrack const_expr T_rbrack {
              (* the rest *)
              match ($3.e_place, $3.e_typ) with
              | (Q_int v, TYPE_int) -> $1 @ [Q_int v]
              | _ ->
                error "parameter array dimension is not an integer constant";
                (* TODO what if error? *)
                []
            }
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
            let esv = {
              e_place = (Q_int $1);
              e_typ = TYPE_int;
            } in esv
        }
     | T_float_const {
            let esv = {
              e_place = (Q_real $1);
              e_typ = TYPE_REAL;
            } in esv
        }
     | T_char_const {
            let esv = {
              e_place = (Q_char $1);
              e_typ = TYPE_char;
            } in esv
        }
     | T_string_literal {
            let esv = {
              e_place = (Q_string $1);
              e_typ = TYPE_array(TYPE_char, String.length $1);
            } in esv
        }
     | T_true {
            let esv = {
              e_place = (Q_bool true);
              e_typ = TYPE_bool;
            } in esv
        }
     | T_false {
            let esv = {
              e_place = (Q_bool false);
              e_typ = TYPE_bool;
            } in esv
        }
     | T_lparen expr T_rparen       { $2 }
     | l_value                      { $1 }
     | call                         { esv_err (* TODO: complete when done with functions *) }
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
