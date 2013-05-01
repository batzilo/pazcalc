all : lexer.ml

%.ml : %.mll
	ocamllex $^

.PHONY : clean

clean :
	$(RM) *.ml
