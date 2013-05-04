/* file: parser.mly */

%{
  open Printf
  open Lexing
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

%left	T_lg_or T_or
%left 	T_lg_and T_and
%nonassoc T_eq T_neq
%nonassoc T_gr T_ls T_greq T_lseq
%left 	T_plus T_minus
%left 	T_mul T_div T_mod T_MOD
%left	T_lparen
%nonassoc UNARY
/* %nonassoc T_TO T_DOWNTO T_STEP T_sem_col T_rparen T_comma T_rbrack */
%nonassoc ELSE
%nonassoc T_else

%start pazprog
%type <unit> pazprog

/* Grammar follows */

%%

pazprog:	/* empty */		{ }
		| pazprog declaration	{ }
		;

declaration: 	const_def	{ }
		| var_def	{ }
		| routine	{ }
		| program	{ }
		;

const_def :	T_const paztype T_id T_assign const_expr const_def2 T_sem_col 	{ }
		;

const_def2:	/* empty */					{ }
		| const_def2 T_comma T_id T_assign const_expr	{ }
		;

var_def:	paztype var_init var_def2 T_sem_col	{ }
		;

var_def2:	/* empty */			{ }
		| var_def2 T_comma var_init	{ }
		;

var_init:	simple_var_init		{ }
		| matrix_var_init	{ }
		;

simple_var_init: T_id 			{ }
		| T_id T_assign expr	{ }
		;

matrix_var_init: T_id T_lbrack const_expr T_rbrack matrix_var_init2	{ }
		;

matrix_var_init2: /* empty */						{ }
		  | matrix_var_init2 T_lbrack const_expr T_rbrack	{ }
		  ;

routine_header:	routine_header1 T_id T_lparen routine_header2 T_rparen	{ }
		;

routine_header1 : T_PROC		{ }
		| T_FUNC paztype		{ }
		;

routine_header2 : /* empty */				{ }
		| paztype formal routine_header3		{ }
		;

routine_header3 : /* empty */				{ }
		| routine_header3 T_comma paztype formal	{ }
		;

formal:		T_amp T_id					{ }
		| T_id						{ }
		| T_id T_lbrack const_expr T_rbrack formal2	{ }
		| T_id T_lbrack T_rbrack formal2		{ }
		;

formal2 :	/* empty */					{ }
		| formal2 T_lbrack const_expr T_rbrack		{ }
		;

routine :	routine_header routine2		{ }
		;

routine2 :	T_sem_col	{ }
		| block		{ }
		;

program_header : T_PROGRAM T_id T_lparen T_rparen ; 	{ }

program : 	program_header block ;			{ }

paztype : 	T_int 		{ }
		| T_bool	{ }
		| T_char	{ }
		| T_REAL;	{ }

const_expr :	expr ;					{ }

expr : 		T_int_const				{ }
		| T_float_const				{ }
		| T_char_const				{ }
		| T_string_literal			{ }
		| T_true				{ }
		| T_false				{ }
		| T_lparen expr T_rparen		{ }
		| l_value				{ }
		| call					{ }
		| unop expr	%prec UNARY		{ }
		| binop_expr				{ }
		;

l_value :	T_id l_value2				{ }
		;

l_value2 : 	/* empty */				{ }
		| l_value2 T_lbrack expr T_rbrack	{ }
		;

unop :		T_plus				{ }
		| T_minus			{ }
		| T_lg_not			{ }
		| T_not				{ }
		;

binop_expr : 	expr T_plus expr			{ }
		| expr T_minus expr			{ }
		| expr T_mul expr			{ }
		| expr T_div expr			{ }
		| expr T_mod expr			{ }
		| expr T_MOD expr			{ }
		| expr T_eq expr			{ }
		| expr T_neq expr			{ }
		| expr T_ls expr			{ }
		| expr T_gr expr			{ }
		| expr T_lseq expr			{ }
		| expr T_greq expr			{ }
		| expr T_lg_and expr			{ }
		| expr T_and expr			{ }
		| expr T_lg_or expr			{ }
		| expr T_or expr			{ }
		;

call : 		T_id T_lparen call2 T_rparen 		{ }
		;

call2 : 	/* empty */				{ }
		| expr call3				{ }
		;

call3 : 	/* empty */				{ }
		| call3 T_comma expr			{ }
		;

block : 	T_lbrace block2 T_rbrace		{ }
		;

block2 : 	/* empty */				{ }
		| block2 local_def			{ }
		| block2 stmt				{ }
		;

local_def :	const_def				{ }
		| var_def				{ }
		;

stmt :		T_sem_col						{ }
		| l_value assign expr T_sem_col				{ }
		| l_value T_plus_plus T_sem_col				{ }
		| l_value T_minus_minus T_sem_col			{ }
		| call T_sem_col					{ }
		| T_if T_lparen expr T_rparen stmt	%prec ELSE	{ }
		| T_if T_lparen expr T_rparen stmt T_else stmt		{ }
		| T_while T_lparen expr T_rparen stmt			{ }
		| T_FOR T_lparen T_id T_comma range T_rparen stmt	{ }
		| T_do stmt T_while T_lparen expr T_rparen T_sem_col	{ }
	/* switch ? */
		| T_break T_sem_col					{ }
		| T_continue T_sem_col					{ }
		| T_return T_sem_col					{ }
		| T_return expr T_sem_col				{ }
		| write T_lparen stmt2 T_rparen T_sem_col		{ }
		;

stmt2 : 	/* empty */			{ }
		| format stmt3			{ }
		;

stmt3 : 	/* empty */			{ }
		| stmt3 T_comma format		{ }
		;

assign : 	T_assign			{ }
		| T_plus_assign			{ }
		| T_minus_assign		{ }
		| T_mul_assign			{ }
		| T_div_assign			{ }
		| T_mod_assign			{ }
		;

range : 	expr range2 expr range3		{ }
		;

range2 : 	T_TO 			{ }
		| T_DOWNTO 		{ }
		;

range3 : 	/* empty */			{ }
		| T_STEP expr			{ }
		;

	/* clause */

write:		T_WRITE			{ }
		| T_WRITELN		{ }
		| T_WRITESP		{ }
		| T_WRITESPLN		{ }
		;
		
format : 	expr							{ }
		| T_FORM T_lparen expr T_comma expr format2 T_rparen	{ }
		;

format2 : 	/* empty */		{ }
		| T_comma expr		{ }
		;

%%
