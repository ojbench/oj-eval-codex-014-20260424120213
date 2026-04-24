#pragma once
#ifndef PYTHON_INTERPRETER_EVALVISITOR_H
#define PYTHON_INTERPRETER_EVALVISITOR_H


#include "Python3ParserBaseVisitor.h"
#include <any>
#include <map>
#include <string>
#include <vector>
#include <memory>
#include <iostream>
#include <sstream>
#include <algorithm>
#include <cctype>

// A very lightweight BigInt based on decimal string operations.
// Supports +, -, *, //, %, comparisons, and sign.
// This is not optimized but sufficient for assignment testcases.
class BigInt {
public:
    int sign; // 1, 0, -1
    std::string digits; // most significant first, no leading zeros unless zero

    BigInt(): sign(0), digits("0") {}
    explicit BigInt(long long v) { *this = fromLong(v); }
    explicit BigInt(const std::string &s) { *this = parse(s); }

    static BigInt fromLong(long long v) {
        BigInt r;
        if (v == 0) { r.sign = 0; r.digits = "0"; return r; }
        r.sign = v < 0 ? -1 : 1;
        unsigned long long u = v < 0 ? (unsigned long long)(-v) : (unsigned long long)v;
        r.digits = std::to_string(u);
        return r;
    }
    static BigInt parse(const std::string &s) {
        BigInt r;
        if (s.empty()) { r.sign = 0; r.digits = "0"; return r; }
        size_t i = 0;
        int sg = 1;
        if (s[0] == '+') { sg = 1; i = 1; }
        else if (s[0] == '-') { sg = -1; i = 1; }
        // accumulate digits
        std::string d;
        for (; i < s.size(); ++i) {
            char c = s[i];
            if (c >= '0' && c <= '9') d.push_back(c);
        }
        if (d.empty() || (d.size()==1 && d[0]=='0')) { r.sign = 0; r.digits = "0"; return r; }
        // remove leading zeros
        size_t nz = 0; while (nz < d.size() && d[nz] == '0') ++nz;
        d = nz == d.size() ? std::string("0") : d.substr(nz);
        r.sign = sg;
        r.digits = d;
        if (r.digits == "0") r.sign = 0;
        return r;
    }
    static int cmpAbs(const BigInt &a, const BigInt &b) {
        if (a.digits.size() != b.digits.size()) return a.digits.size() < b.digits.size() ? -1 : 1;
        if (a.digits == b.digits) return 0;
        return a.digits < b.digits ? -1 : 1;
    }
    static BigInt addAbs(const BigInt &a, const BigInt &b) {
        std::string ra; ra.reserve(std::max(a.digits.size(), b.digits.size())+1);
        int i = (int)a.digits.size()-1, j=(int)b.digits.size()-1, carry=0;
        while (i>=0 || j>=0 || carry) {
            int da = i>=0 ? (a.digits[i]-'0') : 0;
            int db = j>=0 ? (b.digits[j]-'0') : 0;
            int s = da + db + carry;
            ra.push_back(char('0' + (s%10)));
            carry = s/10;
            --i; --j;
        }
        std::reverse(ra.begin(), ra.end());
        BigInt r; r.sign = 1; r.digits = ra; return r;
    }
    static BigInt subAbs(const BigInt &a, const BigInt &b) {
        // assume |a| >= |b|
        std::string ra; ra.reserve(a.digits.size());
        int i=(int)a.digits.size()-1, j=(int)b.digits.size()-1, borrow=0;
        while (i>=0) {
            int da = a.digits[i]-'0' - borrow;
            int db = j>=0 ? (b.digits[j]-'0') : 0;
            if (da < db) { da += 10; borrow = 1; } else borrow = 0;
            int d = da - db;
            ra.push_back(char('0' + d));
            --i; --j;
        }
        while (ra.size()>1 && ra.back()=='0') ra.pop_back();
        std::reverse(ra.begin(), ra.end());
        BigInt r; r.sign = (ra=="0"?0:1); r.digits = ra; if (r.digits=="0") r.sign=0; return r;
    }
    friend BigInt operator+(const BigInt &a, const BigInt &b) {
        if (a.sign==0) return b; if (b.sign==0) return a;
        if (a.sign==b.sign) { BigInt t = addAbs(a,b); t.sign = a.sign; if (t.digits=="0") t.sign=0; return t; }
        int c = cmpAbs(a,b);
        if (c==0) return BigInt();
        if (c>0) { BigInt t = subAbs(a,b); t.sign = a.sign; if (t.digits=="0") t.sign=0; return t; }
        BigInt t = subAbs(b,a); t.sign = b.sign; if (t.digits=="0") t.sign=0; return t;
    }
    friend BigInt operator-(const BigInt &a, const BigInt &b) {
        BigInt nb=b; nb.sign = -nb.sign; return a + nb;
    }
    friend BigInt operator*(const BigInt &a, const BigInt &b) {
        if (a.sign==0 || b.sign==0) return BigInt();
        std::vector<int> res(a.digits.size()+b.digits.size(), 0);
        for (int i=(int)a.digits.size()-1;i>=0;--i) {
            int da = a.digits[i]-'0';
            for (int j=(int)b.digits.size()-1;j>=0;--j) {
                int db = b.digits[j]-'0';
                res[i+j+1] += da*db;
            }
        }
        for (int k=(int)res.size()-1;k>0;--k) {
            int carry = res[k]/10; res[k]%=10; res[k-1]+=carry;
        }
        std::string rd; rd.reserve(res.size());
        int p=0; while (p<(int)res.size()-1 && res[p]==0) ++p;
        for (;p<(int)res.size();++p) rd.push_back(char('0'+res[p]));
        BigInt r; r.sign = a.sign*b.sign; r.digits = rd.empty()?"0":rd; if (r.digits=="0") r.sign=0; return r;
    }
    static BigInt divmod(const BigInt &a, const BigInt &b, BigInt &rem) {
        // integer division towards -inf as Python //
        if (b.sign==0) { rem = BigInt(); return BigInt(); }
        if (a.sign==0) { rem = BigInt(); return BigInt(); }
        int sg = a.sign*b.sign;
        // long division on abs
        BigInt A=a; A.sign=1; BigInt B=b; B.sign=1;
        BigInt q; q.sign=0; q.digits="0";
        BigInt r; r.sign=0; r.digits="0";
        std::string out;
        for (char c: A.digits) {
            // r = r*10 + (c-'0')
            if (r.sign==0) { r.sign=1; r.digits="0"; }
            if (r.digits=="0") r.digits = std::string(1, c);
            else r.digits.push_back(c);
            // remove leading zeros
            size_t nz=0; while (nz<r.digits.size() && r.digits[nz]=='0') ++nz;
            r.digits = nz==r.digits.size()?"0":r.digits.substr(nz);
            if (r.digits=="0") r.sign=0; else r.sign=1;
            // find digit
            int lo=0, hi=9, best=0;
            while (lo<=hi) {
                int mid=(lo+hi)/2;
                // mid*B <= r ?
                BigInt mb = BigInt::fromLong(mid);
                BigInt prod = B * mb;
                int cmp = BigInt::cmpAbs(prod.sign==0?BigInt():prod, r);
                if (cmp<=0) { best=mid; lo=mid+1; } else hi=mid-1;
            }
            BigInt pb = B * BigInt::fromLong(best);
            r = r - pb;
            out.push_back(char('0'+best));
        }
        // build quotient
        size_t nz=0; while (nz<out.size() && out[nz]=='0') ++nz;
        std::string qd = nz==out.size()?"0":out.substr(nz);
        q.sign = (qd=="0"?0:sg);
        q.digits = qd;
        rem = r; rem.sign = a.sign; // remainder carries sign of dividend for floor fix
        // Python floor division
        // a = b*q + r with 0<=r<|b| for positive
        // For negative, adjust: if sg<0 and r.sign!=0, q = q - 1; r = r + |b|
        if (sg<0 && rem.sign!=0) {
            q = q - BigInt::fromLong(1);
            rem = rem + B;
            rem.sign = a.sign; // keep sign consistent
        }
        return q;
    }
    friend BigInt operator/(const BigInt &a, const BigInt &b) {
        BigInt r; return divmod(a,b,r);
    }
    friend BigInt operator%(const BigInt &a, const BigInt &b) {
        BigInt r; (void)divmod(a,b,r); return r;
    }
    friend bool operator==(const BigInt &a, const BigInt &b) {
        return a.sign==b.sign && a.digits==b.digits;
    }
    friend bool operator!=(const BigInt &a, const BigInt &b) { return !(a==b); }
    friend bool operator<(const BigInt &a, const BigInt &b) {
        if (a.sign!=b.sign) return a.sign < b.sign;
        if (a.sign==0) return false;
        int ca = cmpAbs(a,b);
        return a.sign>0 ? (ca<0) : (ca>0);
    }
    friend bool operator>(const BigInt &a, const BigInt &b) { return b<a; }
    friend bool operator<=(const BigInt &a, const BigInt &b) { return !(b<a); }
    friend bool operator>=(const BigInt &a, const BigInt &b) { return !(a<b); }
    std::string toString() const {
        if (sign==0) return "0";
        return (sign<0?"-":"") + digits;
    }
};

struct Value {
    enum Type { TNone, TBool, TInt, TFloat, TString } type = TNone;
    bool b = false;
    BigInt i;
    double f = 0.0;
    std::string s;
};

inline Value makeNone(){ return Value{Value::TNone}; }
inline Value makeBool(bool v){ Value x; x.type=Value::TBool; x.b=v; return x; }
inline Value makeInt(const BigInt &v){ Value x; x.type=Value::TInt; x.i=v; return x; }
inline Value makeFloat(double v){ Value x; x.type=Value::TFloat; x.f=v; return x; }
inline Value makeStr(const std::string &v){ Value x; x.type=Value::TString; x.s=v; return x; }

inline double toFloat(const Value &v){
    if (v.type==Value::TFloat) return v.f;
    if (v.type==Value::TInt) return std::stod(v.i.toString());
    if (v.type==Value::TBool) return v.b?1.0:0.0;
    if (v.type==Value::TString) return std::stod(v.s);
    return 0.0;
}
inline BigInt toInt(const Value &v){
    if (v.type==Value::TInt) return v.i;
    if (v.type==Value::TFloat) return BigInt((long long)v.f);
    if (v.type==Value::TBool) return BigInt(v.b?1:0);
    if (v.type==Value::TString) return BigInt(v.s);
    return BigInt();
}
inline bool toBool(const Value &v){
    if (v.type==Value::TBool) return v.b;
    if (v.type==Value::TInt) return v.i.sign!=0;
    if (v.type==Value::TFloat) return v.f!=0.0;
    if (v.type==Value::TString) return !v.s.empty();
    return false;
}
inline std::string toStr(const Value &v){
    if (v.type==Value::TString) return v.s;
    if (v.type==Value::TInt) return v.i.toString();
    if (v.type==Value::TFloat) { std::ostringstream oss; oss.setf(std::ios::fixed); oss.precision(6); oss<<v.f; return oss.str(); }
    if (v.type==Value::TBool) return v.b?"True":"False";
    return "None";
}

struct Frame {
    std::map<std::string, Value> vars;
};

class EvalVisitor : public Python3ParserBaseVisitor {
public:
    std::vector<Frame> stack; // stack[0] = global

    EvalVisitor(){ stack.emplace_back(); }

    std::any visitFile_input(Python3Parser::File_inputContext *ctx) override;
    std::any visitStmt(Python3Parser::StmtContext *ctx) override;
    std::any visitSimple_stmt(Python3Parser::Simple_stmtContext *ctx) override;
    std::any visitSmall_stmt(Python3Parser::Small_stmtContext *ctx) override;
    std::any visitExpr_stmt(Python3Parser::Expr_stmtContext *ctx) override;
    std::any visitFuncdef(Python3Parser::FuncdefContext *ctx) override;
    std::any visitTest(Python3Parser::TestContext *ctx) override;
    std::any visitOr_test(Python3Parser::Or_testContext *ctx) override;
    std::any visitAnd_test(Python3Parser::And_testContext *ctx) override;
    std::any visitNot_test(Python3Parser::Not_testContext *ctx) override;
    std::any visitComparison(Python3Parser::ComparisonContext *ctx) override;
    std::any visitArith_expr(Python3Parser::Arith_exprContext *ctx) override;
    std::any visitTerm(Python3Parser::TermContext *ctx) override;
    std::any visitFactor(Python3Parser::FactorContext *ctx) override;
    std::any visitAtom_expr(Python3Parser::Atom_exprContext *ctx) override;
    std::any visitTrailer(Python3Parser::TrailerContext *ctx) override;
    std::any visitAtom(Python3Parser::AtomContext *ctx) override;
    std::any visitFormat_string(Python3Parser::Format_stringContext *ctx) override;
    std::any visitTestlist(Python3Parser::TestlistContext *ctx) override;
    std::any visitArglist(Python3Parser::ArglistContext *ctx) override;
    std::any visitArgument(Python3Parser::ArgumentContext *ctx) override;
    std::any visitIf_stmt(Python3Parser::If_stmtContext *ctx) override;
    std::any visitWhile_stmt(Python3Parser::While_stmtContext *ctx) override;
    std::any visitSuite(Python3Parser::SuiteContext *ctx) override;
    std::any visitCompound_stmt(Python3Parser::Compound_stmtContext *ctx) override;

private:
    Value getVar(const std::string &name);
    void setVar(const std::string &name, const Value &v);
    Value applyAdd(const Value&a,const Value&b);
    Value applySub(const Value&a,const Value&b);
    Value applyMul(const Value&a,const Value&b);
    Value applyDiv(const Value&a,const Value&b); // float division
    Value applyIDiv(const Value&a,const Value&b); // integer floor division
    Value applyMod(const Value&a,const Value&b);
};


// Implementations are in Evalvisitor.cpp


#endif//PYTHON_INTERPRETER_EVALVISITOR_H
