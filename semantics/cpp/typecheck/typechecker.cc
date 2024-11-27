#include <iostream>
#include <algorithm>
#include "ast.hh"
#include "typechecker.hh"

using namespace std;
using namespace ast;

namespace typechecker {
set<Type> Typechecker::setUnion(set<Type>& set1, set<Type>& set2) {
    vector<Type> unionVector(set1.size() + set2.size());

    auto it = set_union(set1.begin(), set1.end(), set2.begin(),
                    set2.end(), unionVector.begin());

    // Resize the vector to remove the unused elements
    unionVector.resize(it - unionVector.begin());
    set<Type> unionSet(unionVector.begin(), unionVector.end());
    return unionSet;
}

set<Type> Typechecker::setMinus(set<Type>& set1, set<Type>& set2) {
    set<Type> newSet;
    for(auto& el : set1) {
        if(set2.find(el) == set2.end()) {
            newSet.insert(el);
        }
    }
    return newSet;
}

template <typename T> Env<T>::Env(Env<T> *p) : parent(p) {}

template <typename T> Env<T> *Env<T>::getParent() {
    return parent;
}

template <typename T> void Env<T>::print() {
}

template <typename T> T Env<T>::get(string vname) {

    if(table.find(vname) != table.end()) {
        return table[vname];
    }
    if(parent != NULL) {
        return parent->get(vname);
    }
    throw ("Variable " + vname + " not found.");
}

template <typename T> void Env<T>::addMapping(string name, T value) {
    if(table.find(name) == table.end()) {
        table[name] = value;
    }
    else {
        string m = "Env::addMapping : repeat declaration for name " + name + ".";
        throw m;
    }
}

template <typename T> Env<T>::~Env() {}

ValueEnv::ValueEnv(ValueEnv *p) : Env(p) {}

void ValueEnv::print() {
    for(auto &d : table) {
        
        cout << d.first << " : " << d.second << endl;
    }
}

vector<Type> FunctionTypeSig::getParameterTypes() {
    return parameterTypes;
}

Type FunctionTypeSig::getReturnType() {
    return returnType;
}

FunctionTypeSig::FunctionTypeSig(vector<Type>& partypes, Type rettype) {

    for(auto& ptype : partypes) {
        parameterTypes.push_back(ptype);
    }
    returnType = rettype;
}

FunctionTypeSig::FunctionTypeSig() {}

FunctionTypeSig& FunctionTypeSig::operator=(FunctionTypeSig& fsig) {
    for(auto& ptype : fsig.parameterTypes) {
        parameterTypes.push_back(ptype);
    }
    returnType = fsig.returnType;
    return *this;
}

string FunctionTypeSig::toString() {
    string sig = "(";
    for(auto& partype : parameterTypes) {
        sig += to_string(partype) + ", ";
    }
    sig += ") ===> ";
    sig += to_string(returnType);
    return sig;
}

FunctionEnv::FunctionEnv(FunctionEnv *p) : Env(p) {}
FunctionEnv::FunctionEnv() : Env(NULL) {}

void FunctionEnv::print() {
    for(auto &d : table) {
        
        cout << d.first << " : " << d.second.toString() << endl;
    }
}

Typechecker::Typechecker() {
    valueEnv = new ValueEnv(NULL);
}

Typechecker::~Typechecker() {
    delete valueEnv;
}

ValueEnv& Typechecker::getValueEnv() {
    return *valueEnv;
}

FunctionEnv& Typechecker::getFunctionEnv() {
    return functionEnv;
}

void Typechecker::enterScope() {
    ValueEnv *newScope = new ValueEnv(valueEnv);
    valueEnv = newScope;
}

void Typechecker::exitScope() {
    ValueEnv *newScope = dynamic_cast<ValueEnv *>(valueEnv->getParent());
    delete valueEnv;
    valueEnv = newScope;
}

Type Typechecker::typecheckVar(Var& v) {
    return valueEnv->get(v.name);
}

Type Typechecker::typecheckExpression(Expression& e) {
    switch(e.exprtype) {
        case EMPTY:
            return typecheckEmpty(dynamic_cast<Empty&>(e));
        case VAR:
            return typecheckVar(dynamic_cast<Var&>(e));
        case NUM:
            return typecheckNum(dynamic_cast<Num&>(e));
        case BINARY:
            return typecheckBinaryExpression(dynamic_cast<BinaryExpression&>(e));
        case FUNCTIONCALL:
            return typecheckFunctionCall(dynamic_cast<FunctionCall&>(e));
        default:
        string m = "Typechecker::typecheckExpression : Unknown expression type!" + to_string(e.exprtype);
            throw m;
    }
}

Type Typechecker::typecheckEmpty(Empty& e) {
    return VOID;
}

Type Typechecker::typecheckNum(Num& n) {
    return INT;
}

Type Typechecker::typecheckBinaryExpression(BinaryExpression& e) {
    if((typecheckExpression(e.getLeft()) == INT) &&
            (typecheckExpression(e.getRight()) == INT)) {
        return INT;
    }
    throw "Typechecker::typecheckBinaryExpression : Incorrect operand type!";
}

Type Typechecker::typecheckFunctionCall(FunctionCall& funccall) {
    FunctionTypeSig fsig = getFunctionEnv().get(funccall.getName());
    vector<Expression *> args = funccall.getArguments();
    vector<Type> ptypes = fsig.getParameterTypes();
    if(ptypes.size() != args.size()) {
        throw "Typechecker::typecheckFunctionCall : function call " + funccall.getName() + " #arguments unequal to #parameters.";
    }
    for(unsigned int i = 0; i < args.size(); i++) {
        if(typecheckExpression(*(args[i])) != ptypes[i]) {
                throw "Typechecker::typecheckFunctionCall : function call " + funccall.getName() + " argument type unequal to parameter type on position " + to_string(i);
        }
    }
    return fsig.getReturnType();
}

set<Type> Typechecker::typecheckStatement(Statement& stmt) {
    switch(stmt.stmttype) {
        case SKIP:
            return typecheckSkip(dynamic_cast<SkipStatement&>(stmt));
        case ASSIGN:
            return typecheckAssignment(dynamic_cast<AssignmentStatement&>(stmt));
        case BRANCH:
            return typecheckBranch(dynamic_cast<BranchStatement&>(stmt));
        case LOOP:
            return typecheckLoop(dynamic_cast<LoopStatement&>(stmt));
        case SEQUENCE:
            return typecheckSequence(dynamic_cast<SequenceStatement&>(stmt));
        case BLOCK:
            return typecheckBlock(dynamic_cast<BlockStatement&>(stmt));
        case RETURN:
            return typecheckReturn(dynamic_cast<ReturnStatement&>(stmt));
        default:
            throw "Typechecker::typecheckStatement : Unknown statement type!";
    }
}

set<Type> Typechecker::typecheckSkip(SkipStatement& stmt) {
    return {NIL};
}

set<Type> Typechecker::typecheckAssignment(AssignmentStatement& stmt) {
    if(valueEnv->get(stmt.getVariable()) ==
            typecheckExpression(*(stmt.getExpression()))) {
        return {NIL};
    }
    throw "Typechecker::typecheckAssignmentStatement : type mismatch!";
}

set<Type> Typechecker::typecheckBranch(BranchStatement& branch) {
    if(typecheckExpression(branch.getCondition()) != BOOL) {
        throw "Typechecker::typecheckBranchStatement : condition doesn't typecheck.";
    }
    set<Type> thenTypes = typecheckStatement(branch.getThenStatement());
    set<Type> elseTypes = typecheckStatement(branch.getElseStatement());
    if(thenTypes == elseTypes) {
        return thenTypes;
    }

    set<Type> types = setUnion(thenTypes, elseTypes);
    if(types.size() > 2) {
        throw "Typechecker::typecheckBranchStatement : too many types.";
    }
    if(types.size() == 2) {
        if(types.find(NIL) == types.end()) {
            throw "Typechecker::typecheckBranchStatement : type set doesn't have NIL in it.";
        }
    }
    return types;
}

set<Type> Typechecker::typecheckLoop(LoopStatement& loop) {
    set<Type> bodyTypes = typecheckStatement(loop.getBody());
    set<Type> elseTypes = {NIL};
    set<Type> loopTypes = setUnion(bodyTypes, elseTypes);
    return loopTypes;
}

set<Type> Typechecker::typecheckSequence(SequenceStatement& stmt) {
    vector<Statement *>& statements = stmt.getStatements();
    vector<set<Type>> types;
    set<Type> nilType = {NIL};
    for(auto& statement : statements) {
        types.push_back(typecheckStatement(*statement));
    }
    // To check the reachability of remaining statements
    for(unsigned int i = 0; i < types.size() - 1; i++) { 
        if(types[i].size() == 1 && types[i] != nilType) {
            throw "Typechecker::typecheckSequence : some non-final statement has only one type that's not NIL.";
        }
        // remaining statements will be unreachable.
        if(types[i].size() == 2 && types[i].find(NIL) == types[i].end()) {
            throw "Typechecker::typecheckSequence : some non-final statement has no type NIL.";
        }
    }
    set<Type> otherTypes;
    for(unsigned int i = 0; i < types.size(); i++) {
        otherTypes = setUnion(otherTypes, types[i]);
    }
    otherTypes = setMinus(otherTypes, nilType);
    // there should be maximum only one non-nil type returned from the sequence.
    if(otherTypes.size() > 1) {
        throw "Typechecker::typecheckSequence : Too many non-nil types.";
    }
    return setUnion(otherTypes, types[types.size() - 1]);
}

set<Type> Typechecker::typecheckBlock(BlockStatement& stmt) {
    enterScope();
    for(auto& declaration : stmt.getDeclarations()) {
        valueEnv->addMapping(declaration->getVariable(), declaration->getType());
    }
    set<Type> types = typecheckStatement(stmt.getStatement());
    exitScope();
    return types;
}

set<Type> Typechecker::typecheckReturn(ReturnStatement& stmt) {
    set<Type> returnTypes = {typecheckExpression(stmt.getExpression())};
    return returnTypes;
}

void Typechecker::typecheckFunctionDef(FunctionDefinition& fdef) {
    enterScope();
    for(auto& p : fdef.getParameters()) {
        valueEnv->addMapping(p->getVariable(), p->getType());
    }
    set<Type>bodyTypes = typecheckBlock(fdef.getBody());
    exitScope();
    if(bodyTypes.size() != 1) {
        throw "Typechecker::typecheckFunctionDef : too many types from" \
        "function body" +  fdef.getName() + ".";
    }
    auto first = bodyTypes.begin();
    Type bodyType = *first;
    Type returnType = fdef.getReturnType();
    if((bodyType == VOID || bodyType == NIL) && returnType != VOID) {
        throw "Typechecker::typecheckFunctionDef : return type of " + \
        fdef.getName() + "does not match body type.";
    }
    if(bodyType != returnType) {
        throw "Typechecker::typecheckFunctionDef : return type of " + \
        fdef.getName() + "does not match body type.";
    }
    vector<Type> ptypes;
    for(auto& p : fdef.getParameters()) {
        ptypes.push_back(p->getType());
    }
    FunctionTypeSig fsig(ptypes, fdef.getReturnType());
    getFunctionEnv().addMapping(fdef.getName(), fsig);
}

void Typechecker::typecheckProgram(Program& program) {
    for(auto& d : program.getDeclarations()) {
        valueEnv->addMapping(d->getVariable(), d->getType());
    }
    for(auto& fdef : program.getFunctionDefs()) {
        typecheckFunctionDef(*fdef);
    }
    typecheckStatement(program.getStatement());
}

} // namespace typechecker
