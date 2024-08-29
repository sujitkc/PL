#ifndef AST_HH
#define AST_HH

#include <string>
#include <map>
#include <vector>

using namespace std;

namespace ast {
/*
class SymbolTable {
    private:
        SymbolTable *parent;
        vector<SymbolTable *> children;
        map<string, int> table;
    public:
        SymbolTable(SymbolTable *p);
        SymbolTable();
        int get(string);
        void set(string, int); // will work only on existing variables.
        void add(string, int); // will add/modify an entry on the curren table;
                   // will not recurse to its parent if not found.
        void print();
        ~SymbolTable();
};
*/

class SymbolTable {
    private:
        map<string, int> table;
    public:
        SymbolTable();
        int get(string);
        void set(string, int); // will work only on existing variables.
        void print();
        ~SymbolTable();
};


class Expression {
    public:
        virtual int evaluate() = 0;
        virtual ~Expression() = 0;
};

class Num : public Expression {
    public:
        const int value;
        Num(int v);
        virtual int evaluate();
};

class Var : public Expression {
    public:
        const string name;
        Var(string n);
        virtual int evaluate();
};

class BinaryExpression : public Expression {
    protected:
        Expression *left;
        Expression *right;

    public:
        BinaryExpression(Expression *l, Expression *r);
        virtual ~BinaryExpression();    
};

class AddExpression : public BinaryExpression {
    public:
        AddExpression(Expression *l, Expression *r);
        virtual int evaluate();
};

class SubExpression : public BinaryExpression {
    public:
        SubExpression(Expression *l, Expression *r);
        virtual int evaluate();
};

class MulExpression : public BinaryExpression {
    public:
        MulExpression(Expression *l, Expression *r);
        virtual int evaluate();
};

class DivExpression : public BinaryExpression {
    public:
        DivExpression(Expression *l, Expression *r);
        virtual int evaluate();
};

class Statement {
    public:
        virtual void run() = 0;
        virtual ~Statement() = 0;
};

class AssignmentStatement : public Statement {
    private:
        string variable;
        Expression *expression;
    public:
        void run();
        AssignmentStatement(string, Expression *);
        virtual ~AssignmentStatement();
};

class SequenceStatement : public Statement {
    private:
        vector<Statement *> statements;
    public:
        void addStatement(Statement *s);
        void run();
        virtual ~SequenceStatement();
};

class Program {
    private:
        string name;
        Statement *statement;
        SymbolTable *symbolTable;
    public:
        Program(string, Statement *);
        string getName();
        SymbolTable& getSymbolTable();
        void run();
        ~Program();
};

extern Program *theProgram;
} // namespace ast
#endif // AST_HH
