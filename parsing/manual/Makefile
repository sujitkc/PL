rdparser : lexer.cmx rdparser.cmx
	ocamlopt -o rdparser lexer.cmx rdparser.cmx

lexer.cmx : lexer.ml
	ocamlopt -c lexer.ml

lexer.cmi : lexer.ml
	ocamlopt -c lexer.ml

lexer.ml : lexer.mll
	ocamllex lexer.mll

rdparser.cmx : lexer.cmi rdparser.ml
	ocamlopt -c rdparser.ml

clean:
	rm *.cm[ix] lexer.ml *.o rdparser a.out
