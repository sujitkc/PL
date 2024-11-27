CC=g++ -g

driver : driver.o typechecker.o ast.o
	$(CC) -o driver driver.o typechecker.o ast.o

driver.o : driver.cc ast.hh
	$(CC) -c driver.cc

typechecker.o : typechecker.cc typechecker.hh ast.hh
	$(CC) -c typechecker.cc

ast.o : ast.cc ast.hh
	$(CC) -c ast.cc

clean :
	rm *.o driver
