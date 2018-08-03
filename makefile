all	: mysudoku mysudoku.opt

mysudoku.opt	: mysudoku.cmx
	ocamlopt bigarray.cmxa $< -o $@

mysudoku	: mysudoku.cmo
	ocamlc -g bigarray.cma $< -o $@

tests	: all
	for file in samples/puzzle*; do echo $$file 1>&2; ./mysudoku.opt $$file; done;

clean	:
	rm -f *.cm* *.o

dclean	: clean
	rm -f mysudoku.opt mysudoku

%.cmo	: %.ml
	ocamlc -g -c $<

%.cmx	: %.ml
	ocamlopt -c $<

