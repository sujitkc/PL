#include <iostream>
#include "ast.hh" 

using namespace std;

namespace ast {

Expression::~Expression() {}

Num::Num(int v) : value(v) {}

Var::Var(string n) : name(n) {}

BinaryExpression::BinaryExpression(Expression *l, Expression *r) : left(l), right(r){}
BinaryExpression::~BinaryExpression() {
    delete left;
    delete right;
}

AddExpression::AddExpression(Expression *l, Expression *r) : BinaryExpression(l, r) {}

SubExpression::SubExpression(Expression *l, Expression *r) : BinaryExpression(l, r) {}

MulExpression::MulExpression(Expression *l, Expression *r) : BinaryExpression(l, r) {}

DivExpression::DivExpression(Expression *l, Expression *r) : BinaryExpression(l, r) {}

FunctionCall::FunctionCall(string n, vector<Expression *>a) : name(n), arguments(a) {}

FunctionCall::~FunctionCall() {
    for(auto& arg : arguments) {
        delete arg;
    }
}

Statement::~Statement() {}

AssignmentStatement::AssignmentStatement(string vname, Expression *e) :
           variable(vname), expression(e) {}


AssignmentStatement::~AssignmentStatement() {
    delete(expression);
}

void SequenceStatement::addStatement(Statement *s) {
    statements.push_back(s);
}

SequenceStatement::~SequenceStatement() {
    for(auto &s : statements) {
        delete s;
    }
}

BlockStatement::BlockStatement(DeclarationList *d, Statement *s) : declarations(d), statement(s) {}

BlockStatement::~BlockStatement() {
    delete declarations;
    delete statement;
}

FunctionDefinition::FunctionDefinition(Type *t, string n,
        DeclarationList *d, BlockStatement *b) :
    returnType(t), name(n), parameters(d), body(b) {}

FunctionDefinition::~FunctionDefinition() {
    delete parameters;
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

SymbolTable::SymbolTable(SymbolTable *p) : parent(p) {}

SymbolTable *SymbolTable::getParent() {
    return parent;
}

void SymbolTable::print() {
    for(auto &pair : table) {
        string name = pair.first;
        cout << name << " : " << table[name] << endl;
    }
}

int SymbolTable::get(string vname) {
    if (table.find(vname) == table.end()) {
        throw "SymbolTable::get : Variable " + vname + " not found.";
    }
    else {
        return table[vname];
    }
}

void SymbolTable::set(string vname, int value) {
    table[vname] = value;
}

SymbolTable::~SymbolTable() {
}


Program::Program(string n, DeclarationList *dl,
        FunctionDefinitionList *fdef, Statement *s)
    : name (n), declarations(dl), functionDefinitions(fdef), statement (s) {
    symbolTable = new SymbolTable(NULL);
}

string Program::getName() {
    return name;
}

SymbolTable& Program::getSymbolTable() {
    return *symbolTable;
}

void Program::enterScope() {
    SymbolTable *newScope = new SymbolTable(symbolTable);
    symbolTable = newScope;
}

void Program::exitScope() {
    SymbolTable *newScope = symbolTable->getParent();
    delete symbolTable;
    symbolTable = newScope;
}

Program::~Program() {
    delete symbolTable;
    delete declarations;
    delete functionDefinitions;
    delete statement;
}

Program *theProgram;
} // namespace ast
