#
# UPDATE: can use ocamlbuild instead of make
#

# Makefile for Pazcal Compiler

# GNU Make notation:
# $(T) stands for whatever variable T is
# $@ stands for that rule's target
# $^ stands for that rule's all prerequisites

# Define the final Target
T=compiler

# What to build
all : $(T)

# Build the whole compiler by linking all object code files
$(T) : lexer.cmo parser.cmo
	ocamlc -o $@ $^

# compile ecery interface source code file with extension .mli
#%.cmi : %.mli
#	ocamlc -c $^

# Compile every source code file with extension .ml to object code
%.cmo : %.ml
	ocamlc -c $^

# Build the lexical analyzer source code (lexer.ml)
# from lexer.mll: a description file with regular expressions
lexer.ml : lexer.mll parser.cmo
	ocamllex lexer.mll

# Build the parser source code (parser.ml and parser.mli)
# from parser.mly: a grammar description file
# -v : verbose
parser.ml : parser.mly
	ocamlyacc -v parser.mly
# remove parser.mli for now, it'll be created again when parser.ml is compiled
	rm parser.mli


# PHONY targets
.PHONY : clean distclean

# remove all object code files
clean :
	$(RM) *.cmo *.cmi lexer.ml parser.mli parser.ml *.output

# remove all object code files along with the final executable
distclean : clean
	$(RM) $(T)
