all	: mysudoku mysudoku.opt

mysudoku.opt	: mysudoku.cmx
	ocamlopt bigarray.cmxa $< -o $@

mysudoku	: mysudoku.cmo
	ocamlc -g bigarray.cma $< -o $@

tests	: all
	for file in samples/puzzle*.dat; do echo $$file 1>&2; ./mysudoku.opt $$file > $$file.res; done;

clean	:
	rm -f *.cm* *.o

dclean	: clean
	rm -f mysudoku.opt mysudoku

%.cmo	: %.ml
	ocamlc -g -c $<

%.cmx	: %.ml
	ocamlopt -c $<

