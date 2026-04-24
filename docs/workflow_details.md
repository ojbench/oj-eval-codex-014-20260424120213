# Interpreter Assignment Implementation Workflow

## Step 1. Environment Setup

See documentation [antlr_guide.md](antlr_guide.md).

## Step 2. Read "./resources/Python3Parser.g4"

We assume there are such grammar rules (not part of our assignment):

```
plus: NUMBER '+' NUMBER;
NUMBER: [0-9]+;
ADD: '+';
```

## Step 3. Read "./generated/Python3Parser.h"

You can see the following code (corresponding to the above grammar rules):

```cpp
//...............
class PlusContext : public antlr4::ParserRuleContext {
public:
    PlusContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<antlr4::tree::TerminalNode *> NUMBER();
    antlr4::tree::TerminalNode* NUMBER(size_t i);
    antlr4::tree::TerminalNode* ADD();

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;

    virtual std::any accept(antlr4::tree::ParseTreeVisitor *visitor) override;
}
//...............
```

Because in the above `plus` grammar, `NUMBER` must appear at least once, `PlusContext` has the following two functions:

```cpp
std::vector<antlr4::tree::TerminalNode *> NUMBER();
antlr4::tree::TerminalNode* NUMBER(size_t i);
```

The first function returns a `vector` containing pointers to all `NUMBER` tokens. The second function returns a pointer to the i-th `NUMBER` token, starting from 0.

Because `ADD` only appears once in `plus`, it only has the following function, returning a pointer to the unique `ADD` token:

```cpp
antlr4::tree::TerminalNode* ADD()
```

For a `terminal node`, there are the following methods:

```cpp
//...............
std::string toString()
Token* TerminalNodeImpl::getSymbol()
/*
 * for example, consider:
 * antlr4::tree::TerminalNode *it;
 * it->toString() returns the string, for example, "123456" or "a"
 * (so you need to convert (std::string)"123456" to (int)123456)
 * it->getSymbol()->getTokenIndex() returns where this word is in the whole input.
 */
//...............
```

## Step 4. Complete "./src/Evalvisitor.h"

In this step, what you need to do is complete the relevant code:

```cpp
//...............
std::any visitPlus(Python3Parser::PlusContext *ctx)
{
    /*
     * TODO
     * the pseudo-code is:
     * return visit(ctx->NUMBER(0)) + visit(ctx->NUMBER(1));
     */
}
//...............
```

When you write:

```cpp
visit(ctx->NUMBER(0))
```

It is equivalent to writing:

```cpp
visitAtom(ctx->NUMBER(0))
```

So we only need to use the `visit` function to access various nodes, rather than using `visitBalabala`. Think about why?

## Step 5. Compile the Program

Enter the following commands:

```sh
cmake -B build
cmake --build build
```

If you don't know how to use `cmake`, you can use `CLion` to do this. If you don't know how to do this step, please ask the TA.

## The Magic of std::any

For this class, you only need to know one method: `std::any_cast`.

For example, if you have the following grammar rules:

```
plus: atom '+' atom;
atom: NUMBER | STRING+;
NUMBER: [0-9]+;
STRING: [A-Z]+;
ADD: '+';
```

The `Context` in Parser.h looks like this:

```cpp
//...............
class PlusContext : public antlr4::ParserRuleContext {
public:
    PlusContext(antlr4::ParserRuleContext *parent, size_t invokingState);
    virtual size_t getRuleIndex() const override;
    std::vector<AtomContext*> atom();
    AtomContext* atom(size_t i);
    antlr4::tree::TerminalNode* ADD()

    virtual void enterRule(antlr4::tree::ParseTreeListener *listener) override;
    virtual void exitRule(antlr4::tree::ParseTreeListener *listener) override;

    virtual std::any accept(antlr4::tree::ParseTreeVisitor *visitor) override;
}
// Notice that the atom is AtomContext* type rather than a TerminalNode* type.
// An easy way to tell the difference is: capital letter -> TerminalNode; xxxContext otherwise
//...............
```

Then the corresponding code looks like this:

```cpp
//...............
std::any visitPlus(Python3Parser::PlusContext *ctx)
{
    auto ret1 = visit(ctx->atom(0));
    auto ret2 = visit(ctx->atom(1));
    if (auto v1int = std::any_cast<int>(&ret1), v2int = std::any_cast<int>(&ret2);
        v1int && v2int)
        return *v1int + *v2int;
    else if (auto v1str = std::any_cast<string>(&ret1), v2str = std::any_cast<string>(&ret2);
        v1str && v2str)
        return *v1str + *v2str;
    else
        throw(std::string("unsupported operand type(s) for +: ") + ret1.type().name() + " + " + ret2.type().name()); // not needed
}
//...............
```

We guarantee that the test files have correct syntax, so the last two lines are actually not needed.

The `any_cast` template function can directly convert `any` type to the type you want, but if the conversion fails, it will throw an exception directly.

In the above code, the type we fill in the template is a pointer to the target type, so if the types don't match and the conversion fails, it returns `nullptr`.
