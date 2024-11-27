#ifndef TYPECHECKER
#define TYPECHECKER

#include <set>
#include "ast.hh"

using namespace std;
using namespace ast;

namespace typechecker {
template<typename T> class Env {
    protected:
        map<string, T> table;
        Env<T> *parent;
    public:
        Env(Env<T> *parent);
        virtual Env<T> *getParent();
        virtual T get(string);
        virtual void addMapping(string, T);
        virtual void print() = 0;
        virtual ~Env();
};

class ValueEnv : public Env<Type> {
    public:
        ValueEnv(ValueEnv *parent);
        virtual void print();
};

class FunctionTypeSig {
    private:
        vector<Type> parameterTypes;
        Type returnType;

    public:
        FunctionTypeSig(vector<Type>&, Type);
        FunctionTypeSig();
        vector<Type> getParameterTypes();
        Type getReturnType();
        FunctionTypeSig& operator=(FunctionTypeSig&);
	string toString();
};

class FunctionEnv : public Env<FunctionTypeSig> {
    public:
        FunctionEnv();
        FunctionEnv(FunctionEnv *parent);
        virtual void print();
};

class Typechecker {
    private:
        static set<Type> setUnion(set<Type>&, set<Type>&);
        static set<Type> setMinus(set<Type>&, set<Type>&);
        ValueEnv *valueEnv;
        FunctionEnv functionEnv;

        void enterScope();
        void exitScope();
    public:
	Typechecker();
	~Typechecker();

        ValueEnv& getValueEnv();
        FunctionEnv& getFunctionEnv();

        Type typecheckEmpty(Empty&);
        Type typecheckVar(Var&);
        Type typecheckNum(Num&);
        Type typecheckExpression(Expression&);
        Type typecheckBinaryExpression(BinaryExpression&);
        Type typecheckFunctionCall(FunctionCall&);

        set<Type> typecheckStatement(Statement&);
        set<Type> typecheckSkip(SkipStatement&);
        set<Type> typecheckAssignment(AssignmentStatement&);
        set<Type> typecheckBranch(BranchStatement&);
        set<Type> typecheckLoop(LoopStatement&);
        set<Type> typecheckSequence(SequenceStatement&);
        set<Type> typecheckBlock(BlockStatement&);
        set<Type> typecheckReturn(ReturnStatement&);

        void typecheckFunctionDef(FunctionDefinition&);

	void typecheckProgram(Program&);
};
} // namespace typechecker
#endif
