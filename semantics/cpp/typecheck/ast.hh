#ifndef AST_HH
#define AST_HH

#include <string>
#include <map>
#include <vector>

using namespace std;

namespace ast {

enum Type {
    INT,
    BOOL,
    VOID,
    NIL
};

enum ExpressionType {
    EMPTY,
    VAR,
    NUM,
    BINARY,
    FUNCTIONCALL
};

enum StatementType {
    SKIP,
    ASSIGN,
    BRANCH,
    LOOP,
    SEQUENCE,
    BLOCK,
    RETURN
};


class Declaration {
    private:
        string variable;
        Type type;
    public:
        Declaration(string, Type);
        string getVariable();
        Type getType();
};

class Expression {
    public:
        const ExpressionType exprtype;
    public:
        Expression(ExpressionType);
        virtual ~Expression() = 0;
};

class Empty : public Expression {
    public:
        Empty();
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
        Expression& getLeft();
        Expression& getRight();
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
        string getName();
        vector<Expression *> getArguments();
        virtual ~FunctionCall();
};

class Statement {
    public:
        const StatementType stmttype;
    public:
        Statement(StatementType);
        virtual ~Statement() = 0;
};

class SkipStatement : public Statement {
    public:
        SkipStatement();
        virtual ~SkipStatement();
};

class AssignmentStatement : public Statement {
    private:
        string variable;
        Expression *expression;
    public:
        AssignmentStatement(string, Expression *);
        virtual ~AssignmentStatement();
        string getVariable();
        Expression *getExpression();
};

class SequenceStatement : public Statement {
    private:
        vector<Statement *> statements;
    public:
    SequenceStatement(vector<Statement *>);
    SequenceStatement();
        void addStatement(Statement *s);
        vector<Statement *>& getStatements();
        virtual ~SequenceStatement();
};

class BranchStatement : public Statement {
    private:
        Expression *condition;
        Statement *thenStatement;
        Statement *elseStatement;
    public:
        BranchStatement(Expression *, Statement *, Statement *);
        Expression& getCondition();
        Statement& getThenStatement();
        Statement& getElseStatement();
        virtual ~BranchStatement();
};

class LoopStatement : public Statement {
    private:
        Expression *condition;
        Statement *body;
    public:
        LoopStatement(Expression *, Statement *);
        Expression& getCondition();
        Statement& getBody();
        virtual ~LoopStatement();
};

class BlockStatement : public Statement {
    private:
        vector<Declaration *> declarations;
        Statement *statement;
    public:
        BlockStatement(vector<Declaration *>&, Statement *);
        BlockStatement(Statement *);
        vector<Declaration *>& getDeclarations();
        Statement& getStatement();
        virtual ~BlockStatement();
};

class ReturnStatement : public Statement {
    private:
        Expression *expression;
    public:
        ReturnStatement(Expression *);
        Expression& getExpression();
        virtual ~ReturnStatement();
};

class FunctionDefinition {
    private:
        Type returnType;
        string name;
        vector<Declaration *> parameters;
        BlockStatement *body;

    public:
        FunctionDefinition(string, Type, vector<Declaration *>&, BlockStatement *);
        Type getReturnType();
        string getName();
        vector<Declaration *>& getParameters();
        BlockStatement& getBody();
        virtual ~FunctionDefinition();
};

class FunctionDefinitionList {
    private:
        vector<FunctionDefinition *> functionDefinitions;
    public:
        FunctionDefinitionList(vector<FunctionDefinition *>);
        virtual ~FunctionDefinitionList();
};

class Program {
    private:
        string name;
        vector<Declaration *> declarations;
        vector<FunctionDefinition *> functionDefinitions;
        Statement *statement; 

    public:
        Program(string, vector<Declaration *>&, vector<FunctionDefinition *>&, Statement *);
        Program();
        string getName();
        vector<Declaration *>& getDeclarations();
        vector<FunctionDefinition *>& getFunctionDefs();
        Statement& getStatement();
        ~Program();
};

extern Program *theProgram;
} // namespace ast
#endif // AST_HH
