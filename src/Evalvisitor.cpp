#include "Evalvisitor.h"
#include "Python3Parser.h"

using std::any;
using std::any_cast;
using std::string;
using std::vector;
using antlr4::tree::TerminalNode;

namespace {
struct BreakExc{};
struct ContinueExc{};
struct ReturnExc{ vector<Value> values; };

struct Args { vector<Value> pos; std::map<string, Value> kw; };
}

// forward decl
static any callFunction(EvalVisitor* self, const string &fname, const Args &args);

Value EvalVisitor::getVar(const std::string &name) {
    for (int i=(int)stack.size()-1;i>=0;--i) {
        auto it = stack[i].vars.find(name);
        if (it!=stack[i].vars.end()) return it->second;
    }
    return makeNone();
}

void EvalVisitor::setVar(const std::string &name, const Value &v) {
    if (stack.size()>1) {
        // if variable exists in local frame, set local; else set global
        auto &local = stack.back().vars;
        if (local.find(name)!=local.end()) { local[name]=v; return; }
        stack[0].vars[name]=v;
        return;
    }
    stack[0].vars[name]=v;
}

Value EvalVisitor::applyAdd(const Value &a, const Value &b){
    if (a.type==Value::TString && b.type==Value::TString) return makeStr(a.s + b.s);
    if (a.type==Value::TFloat || b.type==Value::TFloat) return makeFloat(toFloat(a)+toFloat(b));
    return makeInt(toInt(a)+toInt(b));
}
Value EvalVisitor::applySub(const Value &a, const Value &b){
    if (a.type==Value::TFloat || b.type==Value::TFloat) return makeFloat(toFloat(a)-toFloat(b));
    return makeInt(toInt(a)-toInt(b));
}
Value EvalVisitor::applyMul(const Value &a, const Value &b){
    // str * int
    if (a.type==Value::TString && (b.type==Value::TInt || b.type==Value::TBool)){
        BigInt bi = toInt(b);
        long long times = 0;
        if (bi.sign<0) times = 0; else times = std::stoll(bi.toString());
        std::string out; out.reserve(a.s.size()* (size_t)times);
        for (long long i=0;i<times;++i) out += a.s;
        return makeStr(out);
    }
    if (b.type==Value::TString && (a.type==Value::TInt || a.type==Value::TBool)) return applyMul(b,a);
    if (a.type==Value::TFloat || b.type==Value::TFloat) return makeFloat(toFloat(a)*toFloat(b));
    return makeInt(toInt(a)*toInt(b));
}
Value EvalVisitor::applyDiv(const Value &a, const Value &b){
    return makeFloat(toFloat(a)/toFloat(b));
}
Value EvalVisitor::applyIDiv(const Value &a, const Value &b){
    return makeInt(toInt(a)/toInt(b));
}
Value EvalVisitor::applyMod(const Value &a, const Value &b){
    return makeInt(toInt(a)%toInt(b));
}

any EvalVisitor::visitFile_input(Python3Parser::File_inputContext *ctx){
    for (auto nl : ctx->NEWLINE()) (void)nl; // ignore
    for (auto st : ctx->stmt()) visit(st);
    return {};
}

any EvalVisitor::visitStmt(Python3Parser::StmtContext *ctx){
    if (ctx->simple_stmt()) return visit(ctx->simple_stmt());
    return visit(ctx->compound_stmt());
}

any EvalVisitor::visitSimple_stmt(Python3Parser::Simple_stmtContext *ctx){
    return visit(ctx->small_stmt());
}

any EvalVisitor::visitSmall_stmt(Python3Parser::Small_stmtContext *ctx){
    if (ctx->expr_stmt()) return visit(ctx->expr_stmt());
    // flow_stmt
    auto f = ctx->flow_stmt();
    if (f->break_stmt()) throw BreakExc();
    if (f->continue_stmt()) throw ContinueExc();
    if (f->return_stmt()){
        vector<Value> vals;
        if (auto tl = f->return_stmt()->testlist()){
            auto anyv = visit(tl);
            vals = any_cast<vector<Value>>(anyv);
        }
        throw ReturnExc{vals};
    }
    return {};
}

any EvalVisitor::visitExpr_stmt(Python3Parser::Expr_stmtContext *ctx){
    auto tlist0 = any_cast<vector<Value>>(visit(ctx->testlist(0)));
    if (ctx->augassign()){
        // only support single target NAME
        // get name from first atom of first testlist element
        auto left = ctx->testlist(0);
        // extract name text (assume single NAME)
        string name = left->getText();
        // Simplify: name must be a simple identifier
        auto rhsVals = any_cast<vector<Value>>(visit(ctx->testlist(1)));
        Value cur = getVar(name);
        Value rhs = rhsVals.empty()?makeNone():rhsVals[0];
        auto op = ctx->augassign();
        Value res;
        if (op->ADD_ASSIGN()) res = applyAdd(cur,rhs);
        else if (op->SUB_ASSIGN()) res = applySub(cur,rhs);
        else if (op->MULT_ASSIGN()) res = applyMul(cur,rhs);
        else if (op->DIV_ASSIGN()) res = applyDiv(cur,rhs);
        else if (op->IDIV_ASSIGN()) res = applyIDiv(cur,rhs);
        else if (op->MOD_ASSIGN()) res = applyMod(cur,rhs);
        else res = rhs;
        setVar(name,res);
        return {};
    }
    // chained assignment or pure expression
    if (ctx->ASSIGN().size()>0){
        // For each assignment pair left = right sequentially
        for (size_t i=0;i<ctx->ASSIGN().size();++i){
            auto lhs = ctx->testlist(i);
            auto rhs = any_cast<vector<Value>>(visit(ctx->testlist(i+1)));
            // Assign sequentially by position
            // Extract LHS names by splitting text by comma (simplified)
            string ltext = lhs->getText();
            // split by ','
            vector<string> names; string cur;
            for (char c: ltext){ if (c==','){ if(!cur.empty()) names.push_back(cur); cur.clear(); } else { if(!isspace((unsigned char)c)) cur.push_back(c);} }
            if (!cur.empty()) names.push_back(cur);
            if (names.empty() && !ltext.empty()) names.push_back(ltext);
            for (size_t k=0; k<names.size() && k<rhs.size(); ++k) setVar(names[k], rhs[k]);
        }
        return {};
    }
    // expression statement only: evaluate and discard
    return {};
}

any EvalVisitor::visitTest(Python3Parser::TestContext *ctx){ return visit(ctx->or_test()); }

any EvalVisitor::visitOr_test(Python3Parser::Or_testContext *ctx){
    Value res = makeBool(false);
    for (size_t i=0;i<ctx->and_test().size();++i){
        Value v = any_cast<Value>(visit(ctx->and_test(i)));
        if (i==0) res = v;
        if (toBool(res)) return res;
        res = v;
    }
    return res;
}

any EvalVisitor::visitAnd_test(Python3Parser::And_testContext *ctx){
    Value res = makeBool(true);
    for (size_t i=0;i<ctx->not_test().size();++i){
        Value v = any_cast<Value>(visit(ctx->not_test(i)));
        if (i==0) res = v;
        if (!toBool(res)) return res;
        res = v;
    }
    return res;
}

any EvalVisitor::visitNot_test(Python3Parser::Not_testContext *ctx){
    if (ctx->NOT()){
        Value v = any_cast<Value>(visit(ctx->not_test()));
        return makeBool(!toBool(v));
    }
    return visit(ctx->comparison());
}

any EvalVisitor::visitComparison(Python3Parser::ComparisonContext *ctx){
    if (ctx->comp_op().empty()){
        return visit(ctx->arith_expr(0));
    }
    vector<Value> vals; vals.reserve(ctx->arith_expr().size());
    for (auto ae: ctx->arith_expr()) vals.push_back(any_cast<Value>(visit(ae)));
    for (size_t i=1;i<vals.size();++i){
        auto op = ctx->comp_op(i-1);
        Value a = vals[i-1], b = vals[i];
        bool truth=false;
        if (op->EQUALS() || op->NOT_EQ_2()){
            if (a.type==Value::TString && b.type==Value::TString) truth = (a.s==b.s);
            else if (a.type==Value::TString || b.type==Value::TString) truth = false;
            else if (a.type==Value::TFloat || b.type==Value::TFloat) truth = (toFloat(a)==toFloat(b));
            else truth = (toInt(a)==toInt(b));
            if (op->NOT_EQ_2()) truth = !truth;
        } else if (a.type==Value::TString && b.type==Value::TString){
            if (op->LESS_THAN()) truth = a.s < b.s;
            else if (op->GREATER_THAN()) truth = a.s > b.s;
            else if (op->LT_EQ()) truth = a.s <= b.s;
            else if (op->GT_EQ()) truth = a.s >= b.s;
        } else {
            // numeric compare
            if (a.type==Value::TFloat || b.type==Value::TFloat){
                double x=toFloat(a), y=toFloat(b);
                if (op->LESS_THAN()) truth = x<y;
                else if (op->GREATER_THAN()) truth = x>y;
                else if (op->LT_EQ()) truth = x<=y;
                else if (op->GT_EQ()) truth = x>=y;
                else if (op->EQUALS()) truth = x==y;
                else if (op->NOT_EQ_2()) truth = x!=y;
            } else {
                BigInt x=toInt(a), y=toInt(b);
                if (op->LESS_THAN()) truth = x<y;
                else if (op->GREATER_THAN()) truth = x>y;
                else if (op->LT_EQ()) truth = x<=y;
                else if (op->GT_EQ()) truth = x>=y;
                else if (op->EQUALS()) truth = x==y;
                else if (op->NOT_EQ_2()) truth = x!=y;
            }
        }
        if (!truth) return makeBool(false);
    }
    return makeBool(true);
}

any EvalVisitor::visitArith_expr(Python3Parser::Arith_exprContext *ctx){
    Value v = any_cast<Value>(visit(ctx->term(0)));
    for (size_t i=1;i<ctx->term().size();++i){
        auto op = ctx->addorsub_op(i-1);
        Value rhs = any_cast<Value>(visit(ctx->term(i)));
        if (op->ADD()) v = applyAdd(v,rhs);
        else v = applySub(v,rhs);
    }
    return v;
}

any EvalVisitor::visitTerm(Python3Parser::TermContext *ctx){
    Value v = any_cast<Value>(visit(ctx->factor(0)));
    for (size_t i=1;i<ctx->factor().size();++i){
        auto op = ctx->muldivmod_op(i-1);
        Value rhs = any_cast<Value>(visit(ctx->factor(i)));
        if (op->STAR()) v = applyMul(v,rhs);
        else if (op->DIV()) v = applyDiv(v,rhs);
        else if (op->IDIV()) v = applyIDiv(v,rhs);
        else if (op->MOD()) v = applyMod(v,rhs);
    }
    return v;
}

any EvalVisitor::visitFactor(Python3Parser::FactorContext *ctx){
    if (ctx->atom_expr()) return visit(ctx->atom_expr());
    Value v = any_cast<Value>(visit(ctx->factor()));
    if (ctx->ADD()) return v;
    if (ctx->MINUS()){
        if (v.type==Value::TFloat) return makeFloat(-v.f);
        if (v.type==Value::TInt) return makeInt(BigInt::fromLong(0)-v.i);
        return makeInt(BigInt::fromLong(0)-toInt(v));
    }
    return v;
}

any EvalVisitor::visitAtom_expr(Python3Parser::Atom_exprContext *ctx){
    // function call or plain atom
    // If atom is NAME and trailer exists, treat as function call
    if (ctx->trailer() && ctx->atom()->NAME()){
        string fname = ctx->atom()->NAME()->getText();
        Args args; if (ctx->trailer()->arglist()) args = any_cast<Args>(visit(ctx->trailer()->arglist()));
        if (fname=="print"){
            for (size_t i=0;i<args.pos.size();++i){
                // debug: show type
                // std::cerr << "[DBG type=" << args.pos[i].type << "]\n";
                if (i) std::cout<<" ";
                std::cout<<toStr(args.pos[i]);
            }
            std::cout<<"\n";
            return makeNone();
        } else if (fname=="int"){
            Value v = args.pos.empty()?makeNone():args.pos[0];
            return makeInt(toInt(v));
        } else if (fname=="float"){
            Value v = args.pos.empty()?makeNone():args.pos[0];
            return makeFloat(toFloat(v));
        } else if (fname=="str"){
            Value v = args.pos.empty()?makeNone():args.pos[0];
            return makeStr(toStr(v));
        } else if (fname=="bool"){
            Value v = args.pos.empty()?makeNone():args.pos[0];
            return makeBool(toBool(v));
        } else {
            // user-defined
            return callFunction(this, fname, args);
        }
    }
    return visit(ctx->atom());
}

// We need a global function table
namespace {
struct FunctionDef {
    vector<string> params;
    vector<bool> hasDefault;
    vector<Value> defaults;
    Python3Parser::SuiteContext *body = nullptr;
};
static std::map<string, FunctionDef> g_funcs;
}

any EvalVisitor::visitTrailer(Python3Parser::TrailerContext *ctx){ return {}; }

any EvalVisitor::visitAtom(Python3Parser::AtomContext *ctx){
    if (auto n=ctx->NAME()){
        string name = n->getText();
        return getVar(name);
    }
    if (auto num=ctx->NUMBER()){
        string t = num->getText();
        if (t.find('.')!=string::npos) return makeFloat(std::stod(t));
        return makeInt(BigInt::parse(t));
    }
    if (ctx->NONE()) return makeNone();
    if (ctx->TRUE()) return makeBool(true);
    if (ctx->FALSE()) return makeBool(false);
    if (ctx->OPEN_PAREN()){
        return visit(ctx->test());
    }
    if (!ctx->STRING().empty()){
        string out;
        for (auto sTok: ctx->STRING()){
            string t = sTok->getText();
            if (!t.empty() && (t[0]=='"' || t[0]=='\'')) t = t.substr(1,t.size()-2);
            out += t;
        }
        return makeStr(out);
    }
    if (ctx->format_string()) return visit(ctx->format_string());
    return makeNone();
}

any EvalVisitor::visitFormat_string(Python3Parser::Format_stringContext *ctx){
    string out;
    size_t expr_i = 0;
    for (auto ch : ctx->children){
        if (auto tn = dynamic_cast<TerminalNode*>(ch)){
            int ttype = tn->getSymbol()->getType();
            if (ttype==Python3Parser::FORMAT_STRING_LITERAL){
                out += tn->getText();
            } else if (ttype==Python3Parser::OPEN_BRACE){
                // insert evaluated expression
                if (expr_i < ctx->testlist().size()){
                    auto vals = any_cast<vector<Value>>(visit(ctx->testlist(expr_i++)));
                    out += vals.empty()?string(""):toStr(vals[0]);
                }
            } else {
                // ignore other tokens like QUOTATION, CLOSE_BRACE, FORMAT_QUOTATION
            }
        }
    }
    return makeStr(out);
}

any EvalVisitor::visitTestlist(Python3Parser::TestlistContext *ctx){
    vector<Value> vs; vs.reserve(ctx->test().size());
    for (auto t: ctx->test()) vs.push_back(any_cast<Value>(visit(t)));
    return vs;
}

any EvalVisitor::visitArglist(Python3Parser::ArglistContext *ctx){
    Args args;
    for (auto a : ctx->argument()){
        if (a->ASSIGN()){
            string key = a->test(0)->getText();
            Value v = any_cast<Value>(visit(a->test(1)));
            args.kw[key]=v;
        } else {
            args.pos.push_back(any_cast<Value>(visit(a->test(0))));
        }
    }
    return args;
}

any EvalVisitor::visitArgument(Python3Parser::ArgumentContext *ctx){ return {}; }

any EvalVisitor::visitIf_stmt(Python3Parser::If_stmtContext *ctx){
    size_t n = ctx->test().size();
    for (size_t i=0;i<n;++i){
        Value cond = any_cast<Value>(visit(ctx->test(i)));
        if (toBool(cond)) { visit(ctx->suite(i)); return {}; }
    }
    if (ctx->ELSE()){
        visit(ctx->suite(n));
    }
    return {};
}

any EvalVisitor::visitWhile_stmt(Python3Parser::While_stmtContext *ctx){
    while (toBool(any_cast<Value>(visit(ctx->test())))){
        try{
            visit(ctx->suite());
        } catch (const BreakExc&){ break; }
        catch (const ContinueExc&){ continue; }
    }
    return {};
}

any EvalVisitor::visitSuite(Python3Parser::SuiteContext *ctx){
    if (ctx->simple_stmt()) return visit(ctx->simple_stmt());
    for (auto st : ctx->stmt()) visit(st);
    return {};
}

// Function definitions
any EvalVisitor::visitFuncdef(Python3Parser::FuncdefContext *ctx){
    // Collect params and defaults
    FunctionDef fd;
    auto params = ctx->parameters()->typedargslist();
    if (params){
        auto tlist = params;
        size_t m = tlist->tfpdef().size();
        size_t defi = 0;
        for (size_t i=0;i<m;++i){
            string name = tlist->tfpdef(i)->getText();
            fd.params.push_back(name);
            if (defi < tlist->ASSIGN().size()){
                // The grammar associates tests with ASSIGN but distributing is tricky; assume defaults are for the last K params
            }
        }
        // Evaluate default values by scanning tests after ASSIGN tokens
        size_t ac = tlist->ASSIGN().size();
        vector<Value> dvals;
        for (size_t i=0;i<tlist->test().size();++i){ dvals.push_back(any_cast<Value>(visit(tlist->test(i)))); }
        // Mark last dvals.size() params as having defaults
        fd.hasDefault.assign(fd.params.size(), false);
        size_t K = dvals.size();
        fd.defaults.assign(fd.params.size(), makeNone());
        for (size_t i=0;i<K;i++){
            size_t idx = fd.params.size()-K+i;
            fd.hasDefault[idx]=true;
            fd.defaults[idx]=dvals[i];
        }
    }
    fd.body = ctx->suite();
    string fname = ctx->NAME()->getText();
    g_funcs[fname]=fd;
    // Also set a placeholder in globals so name lookup returns something
    stack[0].vars[fname]=makeNone();
    return {};
}

// Override generic visit for compound statements
any EvalVisitor::visitCompound_stmt(Python3Parser::Compound_stmtContext *ctx){
    if (ctx->funcdef()) return visit(ctx->funcdef());
    if (ctx->if_stmt()) return visit(ctx->if_stmt());
    if (ctx->while_stmt()) return visit(ctx->while_stmt());
    return {};
}

// Hook function calls in atom_expr continuation
// Replace earlier placeholder by adding function handling below in visitAtom_expr
static any callFunction(EvalVisitor* self, const string &fname, const Args &args){
    auto it = g_funcs.find(fname);
    if (it==g_funcs.end()) return makeNone();
    const FunctionDef &fd = it->second;
    // build new frame
    self->stack.push_back(Frame{});
    // assign params
    size_t n = fd.params.size();
    for (size_t i=0;i<n;++i){
        Value val = makeNone();
        if (i<args.pos.size()) val = args.pos[i];
        else if (args.kw.find(fd.params[i])!=args.kw.end()) val = args.kw.at(fd.params[i]);
        else if (fd.hasDefault[i]) val = fd.defaults[i];
        self->stack.back().vars[fd.params[i]] = val;
    }
    Value ret = makeNone();
    try{
        self->visit(fd.body);
    } catch (const ReturnExc &re){
        if (!re.values.empty()) ret = re.values[0];
    }
    self->stack.pop_back();
    return ret;
}

// Re-open class method to handle function call (C++ requires full qualification; we already defined visitAtom_expr above; we modify its function body to call here.)
