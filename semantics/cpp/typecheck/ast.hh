#ifndef AST_HH
#define AST_HH

#include <string>
#include <map>
#include <vector>

using namespace std;

namespace ast {
class Type {
    private:
        const string name;
    Type *instance();
    protected:
        Type(string);
    virtual Type *createInstance() = 0;
    public:
        Type *getInstance();
};

class IntType : public Type {
    protected:
        IntType();
    virtual Type *createInstance();
};

class BoolType : public Type {
    protected:
        BoolType();
    virtual Type *createInstance();
};

class Declaration {
    private:
        string variable;
        Type *type;
    public:
        Declaration(string, Type *);
};

class DeclarationList {
    private:
        vector<Declaration *> declarations;
    public:
        DeclarationList(vector<Declaration *>);
        void addDeclaration(Declaration *);
};


class Expression {
    public:
        virtual ~Expression() = 0;
};

class Num : public Expression {
    public:
        const int value;
        Num(int v);
};

class Var : public Expression {
    public:
        const string name;
        Var(string n);
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
};

class SubExpression : public BinaryExpression {
    public:
        SubExpression(Expression *l, Expression *r);
};

class MulExpression : public BinaryExpression {
    public:
        MulExpression(Expression *l, Expression *r);
};

class DivExpression : public BinaryExpression {
    public:
        DivExpression(Expression *l, Expression *r);
};

class FunctionCall : public Expression {
    private:
        string name;
        vector<Expression *> arguments;
    public:
        FunctionCall(string, vector<Expression *>);
        virtual ~FunctionCall();
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

class BlockStatement : public Statement {
    private:
        DeclarationList *declarations;
        Statement *statement;
    public:
        BlockStatement(DeclarationList *, Statement *);
        virtual ~BlockStatement();
};

class FunctionDefinition {
    private:
        Type *returnType;
        string name;
        DeclarationList *parameters;
        BlockStatement *body;

    public:
        FunctionDefinition(Type *, string, DeclarationList *, BlockStatement *);
        virtual ~FunctionDefinition();
};

class FunctionDefinitionList {
    private:
        vector<FunctionDefinition *> functionDefinitions;
    public:
        FunctionDefinitionList(vector<FunctionDefinition *>);
        virtual ~FunctionDefinitionList();
};

class SymbolTable {
    private:
        map<string, int> table;
    SymbolTable *parent;
    public:
        SymbolTable(SymbolTable *parent);
	SymbolTable *getParent();
        int get(string);
        void set(string, int); // will work only on existing variables.
        void print();
        ~SymbolTable();
};

class Program {
    private:
        string name;
        DeclarationList *declarations;
        FunctionDefinitionList *functionDefinitions;
        Statement *statement;
        SymbolTable *symbolTable;
    public:
        Program(string, DeclarationList *, FunctionDefinitionList *, Statement *);
        string getName();
        SymbolTable& getSymbolTable();
	void enterScope();
	void exitScope();
        ~Program();
};

extern Program *theProgram;
} // namespace ast
#endif // AST_HH
