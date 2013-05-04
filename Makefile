T=parser

all : $(T)

$(T) : lexer.cmo $(T).cmo main.cmo
	ocamlc -o $@ $^

%.cmo : %.ml
	ocamlc -c $^

lexer.ml : lexer.mll $(T).cmo
	ocamllex lexer.mll

$(T).ml : $(T).mli
	ocamlc -c $(T).mli

$(T).mli : $(T).mly
	ocamlyacc -v $(T).mly

.PHONY : clean distclean

clean :
	$(RM) *.cmo *.cmi lexer.ml $(T).mli $(T).ml *.output

distclean : clean
	$(RM) $(T)
