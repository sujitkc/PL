#include <iostream>
#include "ast.hh" 

using namespace std;

namespace ast {


Declaration::Declaration(string n, Type t) : variable(n), type(t) {}

string Declaration::getVariable() { return variable; }

Type Declaration::getType() { return type; }

Expression::Expression(ExpressionType et) : exprtype (et) {}
Expression::~Expression() {}

Empty::Empty() : Expression(EMPTY) {}

Num::Num(int v) : Expression(NUM), value(v) {}

Var::Var(string n) : Expression(VAR), name(n) {}

BinaryExpression::BinaryExpression(Expression *l, Expression *r) :
    Expression(BINARY), left(l), right(r){}

BinaryExpression::~BinaryExpression() {
    delete left;
    delete right;
}

Expression& BinaryExpression::getLeft() { return *left; }

Expression& BinaryExpression::getRight() { return *right; }

AddExpression::AddExpression(Expression *l, Expression *r) : BinaryExpression(l, r) {}

SubExpression::SubExpression(Expression *l, Expression *r) : BinaryExpression(l, r) {}

MulExpression::MulExpression(Expression *l, Expression *r) : BinaryExpression(l, r) {}

DivExpression::DivExpression(Expression *l, Expression *r) : BinaryExpression(l, r) {}

FunctionCall::FunctionCall(string n, vector<Expression *>a) : 
    Expression(FUNCTIONCALL), name(n), arguments(a) {}

string FunctionCall::getName() {
    return name;
}

vector<Expression *> FunctionCall::getArguments() {
    return arguments;
}

FunctionCall::~FunctionCall() {
    for(auto& arg : arguments) {
        delete arg;
    }
}

Statement::Statement(StatementType sty) : stmttype (sty) {}

Statement::~Statement() {}

SkipStatement::SkipStatement() : Statement(SKIP) {}

SkipStatement::~SkipStatement() {}

AssignmentStatement::AssignmentStatement(string vname, Expression *e) :
           Statement(ASSIGN), variable(vname), expression(e) {}


AssignmentStatement::~AssignmentStatement() {
    delete(expression);
}

string AssignmentStatement::getVariable() {
    return variable;
}

Expression *AssignmentStatement::getExpression() {
    return expression;
}

SequenceStatement::SequenceStatement(vector<Statement *> s) :
    Statement(SEQUENCE), statements(s) {}

SequenceStatement::SequenceStatement() : Statement(SEQUENCE) {}

void SequenceStatement::addStatement(Statement *s) {
    statements.push_back(s);
}

vector<Statement *>& SequenceStatement::getStatements() {
    return statements;
}

SequenceStatement::~SequenceStatement() {
    for(auto &s : statements) {
        delete s;
    }
}

BlockStatement::BlockStatement(vector<Declaration *>& d, Statement *s) :
    Statement(BLOCK), declarations(d), statement(s) {}

BlockStatement::BlockStatement(Statement *s) :
    Statement(BLOCK), declarations({}), statement(s) {}

vector<Declaration *>& BlockStatement::getDeclarations() {
    return declarations;
}

Statement& BlockStatement::getStatement() {
    return *statement;
}

BlockStatement::~BlockStatement() {
    for(auto& d : declarations) {
        delete d;
    }
    delete statement;
}

BranchStatement::BranchStatement(Expression *c, Statement *t, Statement *e) :
    Statement(BRANCH), condition(c), thenStatement(t), elseStatement(e) {}

Expression& BranchStatement::getCondition() { return *condition; }

Statement& BranchStatement::getThenStatement() { return *thenStatement; }

Statement& BranchStatement::getElseStatement() { return *elseStatement; }

BranchStatement::~BranchStatement() {
    delete condition;
    delete thenStatement;
    delete elseStatement;
}

LoopStatement::LoopStatement(Expression *c, Statement *b) :
    Statement(LOOP), condition(c), body(b) {}

Expression& LoopStatement::getCondition() { return *condition; }

Statement& LoopStatement::getBody() { return *body; }

LoopStatement::~LoopStatement() {
    delete condition;
    delete body;
}

ReturnStatement::ReturnStatement(Expression *e ) :
    Statement(RETURN), expression(e) {}

Expression& ReturnStatement::getExpression() {
    return *expression;
}

ReturnStatement::~ReturnStatement() {
    delete expression;
}

FunctionDefinition::FunctionDefinition(string n, Type t, 
        vector<Declaration *>& d, BlockStatement *b) :
    name(n), returnType(t), parameters(d), body(b) {}

Type FunctionDefinition::getReturnType() {
    return returnType;
}

string FunctionDefinition::getName() {
    return name;
}

vector<Declaration *>& FunctionDefinition::getParameters() {
    return parameters;
}

BlockStatement& FunctionDefinition::getBody() {
    return *body;
}

FunctionDefinition::~FunctionDefinition() {
    for(auto& d : parameters) {
        delete d;
    }
    delete body;
}

FunctionDefinitionList::FunctionDefinitionList(
        vector<FunctionDefinition *>fundefs) :
    functionDefinitions(fundefs) {}

FunctionDefinitionList::~FunctionDefinitionList() {
    for(auto& fundef : functionDefinitions) {
        delete fundef;
    }
}

Program::Program(string n, vector<Declaration *>& dl,
        vector<FunctionDefinition *>& fdef, Statement *s)
    : name (n), declarations(dl), functionDefinitions(fdef), statement (s) {}

Program::Program()
    : name ("default program"), statement (NULL) {
    functionDefinitions = vector<FunctionDefinition *>();
}

string Program::getName() {
    return name;
}

vector<Declaration *>& Program::getDeclarations() {
    return declarations;
}

vector<FunctionDefinition *>& Program::getFunctionDefs() {
    return functionDefinitions;
}

Statement& Program::getStatement() {
    return *statement;
}

Program::~Program() {
    for(auto& d : declarations) {
        delete d;
    }

    for(auto& fdef : functionDefinitions) {
        delete fdef;
    }
    delete statement;
}

Program *theProgram;
} // namespace ast
