#include <iostream>
#include "ast.hh"
#include "typechecker.hh"

using namespace std;
using namespace ast;
using namespace typechecker;
class Test {

    protected:
        virtual Program *makeProgram() = 0;
    public:
        void execute() {
            theProgram = makeProgram();
            cout << "************** typechecking " << theProgram->getName() << "*****************" << endl;
            Typechecker typechecker;
            typechecker.typecheckProgram(*theProgram);
            cout << "Value environment:" << endl;
            typechecker.getValueEnv().print();
            cout << "Function environment:" << endl;
            typechecker.getFunctionEnv().print();
            cout << "***************************************" << endl;
            delete theProgram;
        }
};

/*
Input:
*****************************
    int x
    x := 10
*****************************
*/
class T1 : public Test {
    protected:
        virtual Program *makeProgram() {
            Declaration *d1 = new Declaration("x", INT);
            vector<Declaration *> declarations = { d1 };
            Statement *s = new AssignmentStatement("x", new Num(10));
            vector<FunctionDefinition *> fdefs;
            return new Program("P1", declarations, fdefs, s);
        }
};

/*
Input:
*****************************
    int x
    int y
    x := 10
    y := 1
*****************************
*/
class T2 : public Test {
    protected:
        virtual Program *makeProgram() {
            Declaration *d1 = new Declaration("x", INT);
            Declaration *d2 = new Declaration("y", INT);
            vector<Declaration *> declarations = { d1, d2 };
            Statement *s1 = new AssignmentStatement("x", new Num(10));
            Statement *s2 = new AssignmentStatement("y", new Num(1));
            SequenceStatement *seq1 = new SequenceStatement();
            seq1->addStatement(s1);
            seq1->addStatement(s2);
            vector<FunctionDefinition *> fdefs;
            return new Program("P2", declarations, fdefs, seq1);
        }
};

/*
Input:
*****************************
  int x;
  int add(int x, int y) { return x + y }
  x := add(1, 2)
*****************************
*/
class T3 : public Test {
    protected:
        virtual Program *makeProgram() {
        
            Declaration *d1 = new Declaration("x", INT); // int x
            vector<Declaration *> declarations{d1};

            Declaration *p1 = new Declaration("x", INT); // parameter 1
            Declaration *p2 = new Declaration("y", INT); // parameter 2
            vector<Declaration *> params = {p1, p2}; // parameter list
            Var *v1 = new Var("x");
            Var *v2 = new Var("y");
            AddExpression *e1 = new AddExpression(v1, v2); // x + y
            ReturnStatement *s1 = new ReturnStatement(e1); // return x + y
            BlockStatement *b1 = new BlockStatement(s1); // { return x + y }
            FunctionDefinition *fd1 = 
                new FunctionDefinition("add", INT, params, b1);
                    // int add(int x, int y) { return x + y }
            vector<FunctionDefinition *> fdefs{fd1};
            Num *a1 = new Num(1); // 1
            Num *a2 = new Num(2); // 2
            vector<Expression *> args{a1, a2}; // (1, 2)
            FunctionCall *fc1 = new FunctionCall("add", args); // add(1, 2)
            AssignmentStatement *s2 = 
                new AssignmentStatement("x", fc1); // x := add(1, 2)

            return new Program("P3", declarations, fdefs, s2);
        }
};

int main() {
    vector<Test *> testcases = {
        new T1(),
        new T2(),
        new T3()
    };
    try {
        for(auto& t : testcases) {
            t->execute();
            delete t;
        }
    }
    catch(const string& m) {
        cout << "Typechecker exception : " << m << endl;
    }
    catch(const char* m) {
        cout << "Typechecker exception : " << m << endl;
    }
    catch(...) {
        cout << "Typechecker exception : " << endl;
    }
    return 0;
}
