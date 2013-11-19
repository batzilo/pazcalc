# Makefile

compiler:
	make depend
	make all

.PHONY: clean distclean pack count

# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

# The final executable
EXEFILE=pazcalc$(EXE)

# Source code files
MLFILES=Hashcons.ml Identifier.ml Error.ml Types.ml Symbol.ml Symbtest.ml \
  SemQuad.ml Lexer.ml Parser.ml Main.ml 

# Interface files
MLIFILES=Hashcons.mli Identifier.mli Error.mli Types.mli Symbol.mli SemQuad.mli \
  Parser.mli# Lexer.mli

# Object files. Same as Source files, but with extension .cmo
CMOFILES=$(patsubst %.ml,%.cmo,$(MLFILES))

# Same here, but with extension .cmi
CMIFILES=$(patsubst %.ml,%.cmi,$(MLFILES))

# Same here, but with extension .cmx
CMXFILES=$(patsubst %.ml,%.cmx,$(MLFILES))

# Same here, but with extension .o
OBJFILES=$(patsubst %.ml,%.o,$(MLFILES))

# Files produced by ocamllex and ocamlyacc
PARSERFILES=Parser.ml Parser.mli Parser.output Lexer.ml

# ??
SRCFILES=Makefile extend.ml Lexer.mll Parser.mly \
  $(filter-out Parser.% Lexer.%,$(MLFILES)) \
  $(filter-out Parser.%,$(MLIFILES))

# Some flags
CAMLP5_FLAGS=-pp "camlp5o ./extend.cmo"
OCAMLC_FLAGS=-g
OCAMLOPT_FLAGS=
OCAMLC=ocamlc $(OCAMLC_FLAGS)
OCAMLOPT=ocamlopt $(OCAMLOPT_FLAGS)
OCAMLDEP=ocamldep
INCLUDES=

# Make the Compiler
all: $(EXEFILE)

# Make Symbol Table Test
default: symbtest$(EXE)

symbtest$(EXE): $(filter-out Lexer.cmo Parser.cmo,$(CMOFILES))
	$(OCAMLC) -o $@ $^

extend.cmo: extend.ml
	$(OCAMLC) -pp "camlp5o pa_extend.cmo q_MLast.cmo" -I +camlp5 -c $<

%.cmi: %.mli extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

%.cmo: %.ml %.mli extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

%.cmx: %.ml extend.cmo
	$(OCAMLOPT) $(CAMLP5_FLAGS) -c $<

%.cmo %.cmi: %.ml extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

.PHONY: all clean count depend

$(EXEFILE): Parser.mli Lexer.ml $(CMOFILES)
	$(OCAMLC) -o $@ $(CMOFILES)

Parser.ml Parser.mli: Parser.mly
	ocamlyacc -v Parser.mly

Lexer.ml: Lexer.mll
	ocamllex Lexer.mll

-include .depend

depend: $(MLFILES) $(MLIFILES) extend.cmo
	$(OCAMLDEP) $(CAMLP5_FLAGS) $(INCLUDES) \
          $(filter-out extend.cmo,$^) > .depend

depend-symbtest: Hashcons.ml Identifier.ml Error.ml Types.ml Symbol.ml \
                 Symbtest.ml Hashcons.mli Identifier.mli Error.mli Types.mli \
                 Symbol.mli extend.cmo
	$(OCAMLDEP) $(CAMLP5_FLAGS) $(INCLUDES) \
          $(filter-out extend.cmo,$^) > .depend

clean:
	$(RM) $(CMXFILES) $(CMOFILES) $(CMIFILES) $(OBJFILES) $(EXEFILES) \
           extend.cmi extend.cmo \
           $(patsubst %,%.cm?,$(EXEFILES)) $(PARSERFILES) pplib.cma *~

distclean: clean
	$(RM) $(EXEFILE) symbtest$(EXE) .depend

pack: clean
	tar cvfz pazcalc.tar.gz $(SRCFILES)

bonus.zip: distclean
	zip bonus.zip README Makefile extend.ml \
	    Hashcons.mli Identifier.mli Error.mli Types.mli Symbol.mli \
	    Hashcons.ml Identifier.ml Error.ml Types.ml Symbol.ml Symbtest.ml

bonus.tgz: distclean
	tar cvfz bonus.tgz README Makefile extend.ml \
	    Hashcons.mli Identifier.mli Error.mli Types.mli Symbol.mli \
	    Hashcons.ml Identifier.ml Error.ml Types.ml Symbol.ml Symbtest.ml

count:
	wc -l $(SRCFILES)
