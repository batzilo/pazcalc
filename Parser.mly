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

(* first steps *)
let prologue () =
  printf "Start parsing!\n";
  (* initialize the Symbol Table *)
  initSymbolTable 256;
  (* open the global scope *)
  openScope ()

(* last steps *)
let epilogue () =
  printf "End parsing!\n";
  (* printSymbolTable () *)
  (* close the global scope *)
  closeScope();
  printIntermediateCode ()

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

%nonassoc NOELSE      /*(* pseudo-token. it gives T_ELSE higher precedence *)*/
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
/*(* %type <Semquad.quad_t list> pazprog *)*/
%type <semv_expr> expr
%type <Types.typ> paztype


/*
(*
 * Grammar follows
 *
 * Because we're generating an LALR(1) parser,
 * left-recursive grammar rules are good!
 *
 * TODO: Do not put any identation to semantic brackets until we know it runs smoothly
 * TODO: Location Tracking
 *
 *)
 */

%%

pazprog : /*(* empty *)*/ { }
        | dummy_non_terminal declaration_list T_EOF { epilogue ()  }
		;

dummy_non_terminal : /*(* empty, used only for semantic actions *)*/ { prologue () }

declaration_list : declaration { }
                 | declaration_list declaration { }
                 ;

declaration : const_def { }
            | var_def { List.iter addNewQuad $1 }
            | routine { }
            | program { }
            ;

const_def :	T_const paztype T_id T_assign const_expr T_sem_col {
                (* Semantic-Quads actions for constant definition. name, type, value *)
                sq_cdef $3 $2 $5
                }
          | T_const paztype T_id T_assign const_expr const_def2 T_sem_col {
                (* For every tuple in list, register a new constant *)
                let
                  cdef a (b,c) = sq_cdef b a c 
                in
                  begin
                  (* register the first *)
                  sq_cdef $3 $2 $5;
                  (* register the rest *)
                  List.iter (cdef $2) $6
                  end
                }
          ;

/*(* Return a list of tuples (name, value) *)*/
const_def2 : T_comma T_id T_assign const_expr               { [($2, $4)] }
           | const_def2 T_comma T_id T_assign const_expr    { $1 @ [($3, $5)] }
           ;

var_def : paztype var_init T_sem_col {
                sq_vardef $1 $2
                }
        | paztype var_init var_def2 T_sem_col {
                (*
                sq_vardef $1 $2;
                List.iter (sq_vardef $1) $3
                *)
                let first = (sq_vardef $1 $2) in
                let rest = List.map (sq_vardef $1) $3 in
                let all = first @ (List.concat rest) in
                all
                }
		;

var_def2 : T_comma var_init             { [$2] }
         | var_def2 T_comma var_init    { $1 @ [$3] }
         ;

/*(* Return a list of triplets (name, dims, init) *)*/
var_init : simple_var_init { $1 }
         | matrix_var_init { $1 }
         ;

simple_var_init : T_id                  { ($1, [], esv_err) }
                | T_id T_assign expr    { ($1, [], $3) }
                ;

matrix_var_init : T_id T_lbrack const_expr T_rbrack                     { ($1, [$3.e_place], esv_err) }
                | T_id T_lbrack const_expr T_rbrack matrix_var_init2    { ($1, $3.e_place::$5, esv_err) }
                ;

matrix_var_init2 : T_lbrack const_expr T_rbrack                     { [$2.e_place] }
                 | matrix_var_init2 T_lbrack const_expr T_rbrack    { $1 @ [$3.e_place] }
                 ;

routine_header : T_PROC T_id T_lparen T_rparen {
                        (* i.e PROC foo() *)
                        sq_rout_head $2 TYPE_proc []
                        }
               | T_PROC T_id T_lparen routine_header2 T_rparen {
                        (* i.e PROC foo( int a, char b, REAL c, bool d ) *)
                        sq_rout_head $2 TYPE_proc $4
                        }
               | T_FUNC paztype T_id T_lparen T_rparen {
                        (* i.e FUNC int foo() *)
                        sq_rout_head $3 $2 []
                        }
               | T_FUNC paztype T_id T_lparen routine_header2 T_rparen {
                        (* i.e FUNC int foo( int a, char b, REAL c, bool d ) *)
                        sq_rout_head $3 $2 $5
                        }
               ;

/*(* return a list of tuples (type, (name, mode, dims)) *)*/
routine_header2 : paztype formal                            { [ ($1, $2) ] }
                | routine_header2 T_comma paztype formal    { $1 @ [ ($3, $4) ] }
                ;

/*(* return a triplet (name, pass_mode, dims list) *)*/
/*(* FIXME when parameter is array, it's always passed by reference, right ? *)*/
formal : T_id                                       { ($1, PASS_BY_VALUE, []) }
       | T_amp T_id                                 { ($2, PASS_BY_REFERENCE, []) }
       | T_id T_lbrack T_rbrack                     { ($1, PASS_BY_REFERENCE, [Q_int 0]) } 
       | T_id T_lbrack T_rbrack formal2             { ($1, PASS_BY_REFERENCE, (Q_int 0)::$4) }
       | T_id T_lbrack const_expr T_rbrack          { ($1, PASS_BY_REFERENCE, [$3.e_place]) }
       | T_id T_lbrack const_expr T_rbrack formal2  { ($1, PASS_BY_REFERENCE, $3.e_place::$5) }
       ;

/*(* return an Q_int list *)*/
formal2 : T_lbrack const_expr T_rbrack {
              match ($2.e_place, $2.e_typ) with
              | (Q_int v, TYPE_int) -> [Q_int v]
              | _ ->
                error "parameter array dimension is not an integer constant";
                []
            }
        | formal2 T_lbrack const_expr T_rbrack {
              match ($3.e_place, $3.e_typ) with
              | (Q_int v, TYPE_int) -> $1 @ [Q_int v]
              | _ ->
                error "parameter array dimension is not an integer constant";
                []
            }
        ;

routine : routine_header T_sem_col {
              (* we've seen the header, and no block exists
               * so set function to be forwarded *)
              forwardFunction $1;
              printf "This is a forwarded function\n\n";
              closeScope ();
              rmLastQuad ()
            }
        | routine_header block {
              (* we've seen the header and the block *)
              printSymbolTable ();
              closeScope ();
              let q = Q_endu (Q_entry $1)
              in addNewQuad q
            }
        ;

program_header : T_PROGRAM T_id T_lparen T_rparen   { sq_rout_head $2 TYPE_proc [] }
               ;

program : program_header block {
              (* we've seen the header and the block *)
              printSymbolTable ();
              closeScope ();
              let q = Q_endu (Q_entry $1)
              in addNewQuad q
            }
        ;

paztype : T_int     { TYPE_int }
        | T_bool    { TYPE_bool }
        | T_char    { TYPE_char }
        | T_REAL    { TYPE_REAL }
        ;

const_expr : expr {
                  (* make sure it's really a const *)
                  let cv = const_of_quad $1.e_place in
                  match cv with
                  | CONST_none ->
                     error "const expr isn't really a constant!";
                     esv_err
                  | _ ->
                    $1
                }
           ;

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
     | call                         { $1 }
     | T_plus expr     %prec UNARY  { sq_unop "+" $2 (rhs_start_pos 2) (rhs_end_pos 2) }
     | T_minus expr    %prec UNARY  { sq_unop "-" $2 (rhs_start_pos 2) (rhs_end_pos 2) }
     | T_lg_not expr   %prec UNARY  { sq_unop "!" $2 (rhs_start_pos 2) (rhs_end_pos 2) }
     | T_not expr      %prec UNARY  { sq_unop "!" $2 (rhs_start_pos 2) (rhs_end_pos 2) }
     | expr T_plus expr             { sq_binop $1 "+" $3 (rhs_start_pos 1) (rhs_end_pos 3) }
     | expr T_minus expr            { sq_binop $1 "-" $3 (rhs_start_pos 1) (rhs_end_pos 3) }
     | expr T_mul expr              { sq_binop $1 "*" $3 (rhs_start_pos 1) (rhs_end_pos 3) }
     | expr T_div expr              { sq_binop $1 "/" $3 (rhs_start_pos 1) (rhs_end_pos 3) }
     | expr T_mod expr              { sq_binop $1 "%" $3 (rhs_start_pos 1) (rhs_end_pos 3) }
     | expr T_MOD expr              { sq_binop $1 "%" $3 (rhs_start_pos 1) (rhs_end_pos 3) }
     | expr T_eq expr               { sq_relop $1 "==" $3 (rhs_start_pos 1) (rhs_end_pos 3) }
     | expr T_neq expr              { sq_relop $1 "!=" $3 (rhs_start_pos 1) (rhs_end_pos 3) }
     | expr T_ls expr               { sq_relop $1 "<" $3 (rhs_start_pos 1) (rhs_end_pos 3) }
     | expr T_gr expr               { sq_relop $1 ">" $3 (rhs_start_pos 1) (rhs_end_pos 3) }
     | expr T_lseq expr             { sq_relop $1 "<=" $3 (rhs_start_pos 1) (rhs_end_pos 3) }
     | expr T_greq expr             { sq_relop $1 ">=" $3 (rhs_start_pos 1) (rhs_end_pos 3) }
     | expr T_lg_and expr           { sq_binop $1 "&&" $3 (rhs_start_pos 1) (rhs_end_pos 3) }
     | expr T_and expr              { sq_binop $1 "&&" $3 (rhs_start_pos 1) (rhs_end_pos 3) }
     | expr T_lg_or expr            { sq_binop $1 "||" $3 (rhs_start_pos 1) (rhs_end_pos 3) }
     | expr T_or expr               { sq_binop $1 "||" $3 (rhs_start_pos 1) (rhs_end_pos 3) }
     ;

l_value : T_id              { sq_lvalue $1 [] }
        | T_id l_value2     { sq_lvalue $1 $2 }
		;

l_value2 : T_lbrack expr T_rbrack           { [$2] }
         | l_value2 T_lbrack expr T_rbrack  { $1 @ [$3] }
         ;


call : T_id T_lparen T_rparen           { sq_rout_call $1 [] }
     | T_id T_lparen call2 T_rparen     { sq_rout_call $1 $3 }
     ;

call2 : expr                { [$1] }
      | call2 T_comma expr  { $1 @ [$3] }
      ;

block : T_lbrace T_rbrace           { ssv_empty }
      | T_lbrace block2 T_rbrace    {
          (* FIXME add the quads here!!! *)
          (* List.iter addNewQuad $2.s_code; *)
          (* backpatch $2.s_next (Q_int !quadNext); *)
          (* ssv_empty *)
          $2
        }
      /* error recovery */
      | T_lbrace error T_rbrace     { ssv_err }
      ;

block2 : local_def          {
              (*
              (* List.iter addNewQuad $1.s_code; *)
              backpatch $1.s_next (Q_int (!quadNext + (List.length $1.s_code)));
              let ssv = {
                s_next = [];
                s_code = $1.s_code
              } in ssv
              *)
              List.iter addNewQuad $1.s_code;
              backpatch $1.s_next (Q_int !quadNext);
              let ssv = {
                s_next = [];
                s_code = $1.s_code
              } in ssv
              (* ssv_empty *)
            }
       | stmt               {
              (*
              (* List.iter addNewQuad $1.s_code; *)
              backpatch $1.s_next (Q_int (!quadNext + (List.length $1.s_code)));
              let ssv = {
                s_next = [];
                s_code = $1.s_code
              } in ssv
              *)
              (* List.iter addNewQuad $1.s_code; *)
              backpatch $1.s_next (Q_int !quadNext);
              let ssv = {
                s_next = [];
                s_code = $1.s_code
              } in ssv
              (* ssv_empty *)
            }
       | block2 local_def   {
              (*
              (* List.iter addNewQuad $2.s_code; *)
              backpatch $2.s_next (Q_int (!quadNext + (List.length $2.s_code)));
              let ssv = {
                s_next = [];
                s_code = $1.s_code @ $2.s_code
              } in ssv
              *)
              backpatch $1.s_next (Q_int !quadNext);
              List.iter addNewQuad $2.s_code;
              backpatch $2.s_next (Q_int !quadNext);
              let ssv = {
                s_next = [];
                s_code = $1.s_code @ $2.s_code
              } in ssv
              (* ssv_empty *)
            }
       | block2 stmt        {
              (*
              (* List.iter addNewQuad $2.s_code; *)
              backpatch $2.s_next (Q_int (!quadNext + (List.length $2.s_code)));
              let ssv = {
                s_next = [];
                s_code = $1.s_code @ $2.s_code
              } in ssv
              *)
              backpatch $1.s_next (Q_int !quadNext);
              (* List.iter addNewQuad $2.s_code; *)
              backpatch $2.s_next (Q_int !quadNext);
              let ssv = {
                s_next = [];
                s_code = $1.s_code @ $2.s_code
              } in ssv
              (* ssv_empty *)
            }
       ;

local_def :	const_def   { ssv_empty }
          | var_def     { let ssv = { s_next = []; s_code = $1 } in ssv }
          ;

cond : expr { let (c,qs) = cond_of_expr $1 in let foo = Q_empty in (c,[foo]@[foo]@[foo]@[foo]@[foo]@qs) }
     ;

stmt : T_sem_col { ssv_empty }
     | block { $1 }
     | l_value assign expr T_sem_col {
          let qs = sq_assign $1 $2 $3 in
          List.iter addNewQuad qs;
          let ssv = {
            s_next = [];
            s_code = qs
          } in ssv
        }
     | l_value T_plus_plus T_sem_col {
          let qs = sq_plus_plus $1 in
          let ssv = {
            s_next = [];
            s_code = qs
          } in ssv
        }
     | l_value T_minus_minus T_sem_col {
          let qs = sq_minus_minus $1 in
          let ssv = {
            s_next = [];
            s_code = qs
          } in ssv
        }
     | call T_sem_col {
          let ssv = {
            s_next = [];
            s_code = []
          } in ssv
        }
     | T_if T_lparen cond T_rparen stmt	%prec NOELSE {
          (* handle if then *)
          let (c,qs) = $3 in
          backpatch c.c_true (Q_int (!quadNext - (List.length $5.s_code)));
          (* backpatch c.c_true (Q_int !quadNext); *)
          let l1 = c.c_false in
          (* List.iter addNewQuad $5.s_code; *)
          let l = List.merge compare l1 $5.s_next in
          let ssv = {
            s_next = l;
            s_code = qs @ $5.s_code
          } in ssv
        }
     | T_if T_lparen cond T_rparen stmt T_else stmt {
          (* handle if then else *)
          (*
          let c = $3 in
          backpatch c.c_true (Q_int !quadNext);
          (* List.iter addNewQuad $5.s_code; *)
          let s1_len = List.length $5.s_code in
          let s1_code = $5.s_code in
          let l1 = [!quadNext + s1_len] in
          let q = Q_jump ( Q_backpatch ) in
          (* addNewQuad q; *)
          backpatch c.c_false (Q_int (!quadNext + 1 + s1_len));
          (* List.iter addNewQuad $7.s_code; *)
          let s2_code = $7.s_code in
          let l2 = List.merge compare l1 $5.s_next in
          let l = List.merge compare l2 $7.s_next in
          let code = s1_code @ [q] @ s2_code in
          let ssv = {
            s_next = l;
            s_code = code
          } in ssv
          *)
          ssv_empty
        }
     | T_while T_lparen cond T_rparen stmt { ssv_empty }
     | T_FOR T_lparen T_id T_comma range T_rparen stmt { ssv_empty }
     | T_do stmt T_while T_lparen cond T_rparen T_sem_col { ssv_empty }
     /* switch ? */
     | T_break T_sem_col { ssv_empty }
     | T_continue T_sem_col { ssv_empty }
     | T_return T_sem_col {
          (* handle return *)
          let q = Q_ret in
          addNewQuad q;
          let ssv = {
            s_next = [];
            s_code = [q]
          } in ssv;
        }
     | T_return expr T_sem_col {
          (* handle return *)
          let q1 = Q_assign ( $2.e_place , Q_funct_res) in
          let q2 = Q_ret in
          let ssv = {
            s_next = [];
            s_code = [q1]@[q2]
          } in ssv
        }
     | write T_lparen T_rparen T_sem_col {
          (* handle write *)
          (* FIXME *)
          ssv_empty
        }
     | write T_lparen stmt2 T_rparen T_sem_col {
          (* handle write *)
          (* FIXME *)
          ssv_empty
        }
     /* error recovery */
     | error T_sem_col {
          (* error *)
          ssv_err
        }
     ;

stmt2 : frmt { }
      | stmt2 T_comma frmt { }
      ;

assign : T_assign           { "=" }
       | T_plus_assign      { "+" }
       | T_minus_assign     { "-" }
       | T_mul_assign       { "*" }
       | T_div_assign       { "/" }
       | T_mod_assign       { "%" }
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

frmt : expr { }
       | T_FORM T_lparen expr T_comma expr T_rparen	{ }
       | T_FORM T_lparen expr T_comma expr T_comma expr T_rparen { }
       ;

%%
