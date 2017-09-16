mystream.cmx : mystream.ml mystream.cmi
	ocamlopt -c mystream.ml

state.cmx : mystream.cmi state.cmi state.ml
	ocamlopt -c state.ml

mystream.cmi : mystream.mli
	ocamlc -c mystream.mli

state.cmi : state.mli mystream.cmi
	ocamlc -c state.mli

id.cmx : id.ml id.cmi state.cmi mystream.cmi
	ocamlopt -c id.ml

id.cmi : id.mli state.cmi mystream.cmi
	ocamlc -c id.mli

num.cmx : num.ml num.cmi mystream.cmi state.cmi
	ocamlopt -c num.ml

num.cmi : num.mli state.cmi mystream.cmi
	ocamlc -c num.mli

in.cmx : in.ml in.cmi state.cmi mystream.cmi
	ocamlopt -c in.ml

in.cmi : in.mli state.cmi mystream.cmi
	ocamlc -c in.mli

test_id.cmx : test_id.ml mystream.cmi state.cmi id.cmi
	ocamlc -c id.mli
	ocamlopt -c test_id.ml

test_num.cmx : test_num.ml mystream.cmi state.cmi num.cmi
	ocamlc -c num.mli
	ocamlopt -c test_num.ml

test_in.cmx : test_in.ml mystream.cmi state.cmi in.cmi
	ocamlc -c in.mli
	ocamlopt -c test_in.ml

test_string_stream.cmx : test_string_stream.ml mystream.cmi
	ocamlopt -c test_string_stream.ml

id : id.cmx mystream.cmx state.cmx test_id.cmx 
	ocamlopt -o id mystream.cmx state.cmx id.cmx test_id.cmx

num : num.cmx mystream.cmx state.cmx test_num.cmx
	ocamlopt -o num mystream.cmx state.cmx num.cmx test_num.cmx

in : in.cmx mystream.cmx state.cmx test_in.cmx 
	ocamlopt -o in mystream.cmx state.cmx in.cmx test_in.cmx

string_stream : test_string_stream.cmx mystream.cmx
	ocamlopt -o string_stream mystream.cmx test_string_stream.cmx

lexer.cmx : lexer.ml lexer.cmi mystream.cmi num.cmi id.cmi state.cmi
	ocamlopt -c lexer.ml

lexer.cmi : lexer.mli mystream.cmi num.cmi id.cmi in.cmi state.cmi
	ocamlc -c lexer.mli

test_lexer.cmx : test_lexer.ml lexer.cmi mystream.cmi num.cmi id.cmi in.cmi state.cmi
	ocamlopt -c test_lexer.ml

lexer : mystream.cmx state.cmx num.cmx id.cmx in.cmx lexer.cmx test_lexer.cmx
	ocamlopt -o lexer mystream.cmx state.cmx num.cmx id.cmx in.cmx lexer.cmx test_lexer.cmx

clean:
	rm *.cm[xoi] *.o id num in lexer string_stream
