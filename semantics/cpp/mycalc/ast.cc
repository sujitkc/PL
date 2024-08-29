#include <iostream>
#include "ast.hh" 

using namespace std;

namespace ast {

// SymbolTable::SymbolTable(SymbolTable *p) : parent(p) {}
SymbolTable::SymbolTable() {}

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

Expression::~Expression() {}

Num::Num(int v) : value(v) {}

int Num::evaluate() {
    return value;
}

Var::Var(string n) : name(n) {}

int Var::evaluate() {
    return theProgram->getSymbolTable().get(name);
}

BinaryExpression::BinaryExpression(Expression *l, Expression *r) : left(l), right(r){}
BinaryExpression::~BinaryExpression() {
    delete left;
    delete right;
}

AddExpression::AddExpression(Expression *l, Expression *r) : BinaryExpression(l, r) {}

int AddExpression::evaluate() {
    return left->evaluate() + right->evaluate();
}

SubExpression::SubExpression(Expression *l, Expression *r) : BinaryExpression(l, r) {}
int SubExpression::evaluate() {
    return left->evaluate() - right->evaluate();
}

MulExpression::MulExpression(Expression *l, Expression *r) : BinaryExpression(l, r) {}
int MulExpression::evaluate() {
    return left->evaluate() * right->evaluate();
}

DivExpression::DivExpression(Expression *l, Expression *r) : BinaryExpression(l, r) {}
int DivExpression::evaluate() {
    return left->evaluate() / right->evaluate();
}

Statement::~Statement() {}

AssignmentStatement::AssignmentStatement(string vname, Expression *e) :
           variable(vname), expression(e) {}

void AssignmentStatement::run() {
    theProgram->getSymbolTable().set(variable, expression->evaluate());
}

AssignmentStatement::~AssignmentStatement() {
    delete(expression);
}

void SequenceStatement::addStatement(Statement *s) {
    statements.push_back(s);
}

void SequenceStatement::run() {
    for(auto &s : statements) {
        s->run();
    }
}

SequenceStatement::~SequenceStatement() {
    for(auto &s : statements) {
        delete s;
    }
}

Program::Program(string n, Statement *s) : name (n), statement (s) {
    symbolTable = new SymbolTable();
}

string Program::getName() {
    return name;
}

SymbolTable& Program::getSymbolTable() {
    return *symbolTable;
}

void Program::run() {
    statement->run();
}

Program::~Program() {
    delete symbolTable;
    delete statement;
}

Program *theProgram;
} // namespace ast
