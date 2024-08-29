#include <iostream>
#include <vector>

#include "ast.hh"

using namespace std;
using namespace ast;

class Test {

    protected:
        virtual Program *makeProgram() = 0;
    public:
        void execute() {
            theProgram = makeProgram();
            cout << "executing " << theProgram->getName() << endl;
            theProgram->run();
            theProgram->getSymbolTable().print();
            delete theProgram;
        }
};

class Test_AssignmentStatement1 : public Test {
    protected:
        virtual Program *makeProgram() {
            Statement *s = new AssignmentStatement("x", new Num(10));
            return new Program("P1", s);
        }
};

class Test_AssignmentStatement2 : public Test {
    protected:
        virtual Program *makeProgram() {
            Statement *s1 = new AssignmentStatement("x", new Num(10));
	    Statement *s2 = new AssignmentStatement("y", new Num(5));
	    Statement *s3 = new AssignmentStatement("z", new AddExpression(new Var("x"), new Var("y")));
	    SequenceStatement *slist = new SequenceStatement();
	    slist->addStatement(s1);
	    slist->addStatement(s2);
	    slist->addStatement(s3);
            return new Program("P2", slist);
        }
};

int main() {
    cout << "testing AST ..." << endl;
    Test_AssignmentStatement1 t1;
    Test_AssignmentStatement2 t2;
    vector<Test*> testcases{&t1, &t2};
    for(auto& t : testcases) {
	    t->execute();
    }
    return 0;
}
