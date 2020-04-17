state.cmo : state.cmi state.ml
	ocamlc -c state.ml

mybuffer.cmo : mybuffer.ml mybuffer.cmi
	ocamlc -c mybuffer.ml

mybuffer.cmi : mybuffer.mli
	ocamlc -c mybuffer.mli

state.cmi : state.mli
	ocamlc -c state.mli

id.cmo : id.ml id.cmi state.cmi mybuffer.cmi
	ocamlc -c id.ml

id.cmi : id.mli state.cmi mybuffer.cmi
	ocamlc -c id.mli

num.cmo : num.ml num.cmi mybuffer.cmi state.cmi
	ocamlc -c num.ml

num.cmi : num.mli state.cmi mybuffer.cmi
	ocamlc -c num.mli

in.cmo : in.ml in.cmi state.cmi mybuffer.cmi
	ocamlc -c in.ml

in.cmi : in.mli state.cmi mybuffer.cmi
	ocamlc -c in.mli

test_id.cmi : test_id.mli
	ocamlc -c test_id.mli

test_id.cmo : test_id.cmi test_id.ml mybuffer.cmi state.cmi id.cmi
	ocamlc -c test_id.ml

test_num.cmi : test_num.mli
	ocamlc -c test_num.mli

test_num.cmo : test_num.cmi test_num.ml mybuffer.cmi state.cmi num.cmi
	ocamlc -c test_num.ml

test_in.cmi : test_in.mli
	ocamlc -c test_in.mli

test_in.cmo : test_in.cmi test_in.ml mybuffer.cmi state.cmi in.cmi
	ocamlc -c test_in.ml

test_string_stream.cmo : test_string_stream.ml mybuffer.cmi
	ocamlc -c test_string_stream.ml

id : id.cmo mybuffer.cmo state.cmo test_id.cmo 
	ocamlc -o id mybuffer.cmo state.cmo id.cmo test_id.cmo

num : num.cmo mybuffer.cmo state.cmo test_num.cmo
	ocamlc -o num mybuffer.cmo state.cmo num.cmo test_num.cmo

in : in.cmo mybuffer.cmo state.cmo test_in.cmo 
	ocamlc -o in mybuffer.cmo state.cmo in.cmo test_in.cmo

lexer.cmo : lexer.ml lexer.cmi mybuffer.cmi num.cmi id.cmi state.cmi
	ocamlc -c lexer.ml

lexer.cmi : lexer.mli mybuffer.cmi num.cmi id.cmi in.cmi state.cmi
	ocamlc -c lexer.mli

test_lexer.cmo : test_lexer.ml lexer.cmi mybuffer.cmi num.cmi id.cmi in.cmi state.cmi
	ocamlc -c test_lexer.ml

lexer : mybuffer.cmo state.cmo num.cmo id.cmo in.cmo lexer.cmo test_lexer.cmo
	ocamlc -o lexer mybuffer.cmo state.cmo num.cmo id.cmo in.cmo lexer.cmo test_lexer.cmo

clean:
	rm *.cm[xoi] *.o id num in lexer string_stream
