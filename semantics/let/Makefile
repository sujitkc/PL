evaluate : env.cmx evaluate.cmx expression.cmx parser.cmx lexer.cmx
	ocamlopt -o evaluate env.cmx expression.cmx parser.cmx lexer.cmx evaluate.cmx 

parser.ml : parser.mly
	ocamlyacc parser.mly

parser.mli : parser.mly
	ocamlyacc parser.mly

lexer.ml : lexer.mll parser.mli
	ocamllex lexer.mll

parser.cmx : parser.ml expression.mli
	ocamlopt -c parser.ml

parser.cmi : parser.mli expression.cmi
	ocamlc -c parser.mli

lexer.cmx : lexer.ml parser.cmi
	ocamlopt -c lexer.ml

expression.cmi : expression.mli
	ocamlc -c expression.mli

expression.cmx : expression.mli expression.ml
	ocamlopt -c expression.ml

evaluate.cmx : evaluate.ml parser.cmi lexer.cmx
	ocamlopt -c evaluate.ml

env.cmx : env.ml env.cmi
	ocamlopt -c env.ml

env.cmi : env.mli
	ocamlc -c env.mli

clean:
	rm *.cmx *.cmi *.o evaluate lexer.ml parser.ml parser.mli
