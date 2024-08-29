CC=g++ -g

test_ast : test_ast.o ast.o
	$(CC) -o test_ast test_ast.o ast.o

ast.o : ast.cc ast.hh
	$(CC) -c ast.cc

test_ast.o : test_ast.cc ast.hh
	$(CC) -c test_ast.cc

clean :
	rm *.o test_ast
