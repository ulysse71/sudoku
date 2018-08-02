
mysudoku.opt	: mysudoku.cmx
	ocamlopt bigarray.cmxa $< -o $@

mysudoku	: mysudoku.cmo
	ocamlc -g bigarray.cma $< -o $@

tests	:
	for file in puzzle*; do mysudoku $$file; done;

clean	:
	rm -f *.cm* *.o

dclean	: clean
	rm -f mysudoku.opt mysudoku

%.cmo	: %.ml
	ocamlc -g -c $<

%.cmx	: %.ml
	ocamlopt -c $<

