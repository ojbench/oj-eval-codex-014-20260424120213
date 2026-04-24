# Lexer Embedded Code Explanation

This document explains the embedded C++ code in the Python3Lexer.g4 file. Students who are interested can try to understand it.

## Embedded Code Syntax

There are four types of syntax for inserting C++ code into ANTLR-generated code:

### 1. File Header

```
@header {something}
```

The `something` will be directly copied to the generated header and source files, such as Python3Lexer.h and Python3Lexer.cpp. This is often where you place `#include` directives (C++) or `import` directives (Java).

### 2. Members

```
@lexer::members {something}
```

The content written here will be copied into the generated Lexer class. You can write member variables and member functions here. It's worth noting that you can override base class functions.

### 3. Predicates

```
NEWLINE: {1 + 3 > 4}? '\n';
```

The expression in the brackets is evaluated when trying to match the following rule. The rule is only matched if the predicate expression is true.

### 4. Actions

```
NEWLINE: '\n' { std::cout << "Hello, New Line\n"; }
```

Actions are executed after a successful match.

## Embedded Code We Use

1. Included two header files: `list` and `regex`

2. Added three members: `std::list<antlr4::Token *> tokens`, `std::stack<int> indents`, and `int opened`

3. Added functions: `createDedent`, `make_CommonToken`, `getIndentationCount`, and `atStartOfInput`

4. Overrode the Lexer base class's `emit` and `nextToken` functions

5. Added special behaviors when matching newlines and three types of brackets

## Lexer Workflow

We can see these lines of code in main.cpp:

```cpp
ANTLRInputStream input(std::cin);
Python3Lexer lexer(&input);
CommonTokenStream tokens(&lexer);
tokens.fill();
```

Lexical analysis truly begins after calling the `fill` function.

Without worrying about operations like lazy loading and focusing only on the key points, the workflow is roughly divided into the following steps:

1. The `fill` function calls the `nextToken` function
2. The `nextToken` function calls the `match` function
3. After a successful match, the `match` function calls the `emit` function
4. The `emit` function accepts a `Token` object and stores it in the class member `token`
5. After checking some flags (such as whether to skip the Token), the `nextToken` function returns `token`
6. The `fill` function stores `token` somewhere

It's worth noting that ANTLR uses `unique_ptr` for memory management in the above process. `emit` transfers ownership of the passed parameter to the member `token`, while the `nextToken` function transfers ownership of `token` to the return value.

## Purpose of Embedded Code

Overall, the purpose of these embedded codes is to correctly handle indentation issues.

### Handling Unclosed Brackets

When there are unclosed brackets, all indentation is ignored. The `opened` variable in the code records how many unclosed brackets there currently are.

### Handling Indentation Levels

After each newline, the indentation amount is calculated. If the indentation amount is greater than the current indentation amount, an `INDENT` token is created; if less, a `DEDENT` token is created. A special behavior to note is that if the indentation amount equals the current indentation amount, no token is created, only a newline.

Reading the action code for `NEWLINE`, you can see this line:

```cpp
emit(make_CommonToken(NEWLINE, newLine));
```

Here a TOKEN is manually emitted. Subsequently, if conditions are met, an `INDENT` or `DEDENT` TOKEN may be emitted.

### Multiple Tokens from Single Match

Because in this inherited g4 file, `NEWLINE` matches both the newline and all indentation at the beginning of the new line, this means one `NEWLINE` may produce multiple TOKENs.

The default `emit` function's behavior is to destruct the old TOKEN, so we added the member `tokens` and overrode the `emit` function. For the same reason, we also need to override the `nextToken` function to return TOKENs from `tokens` in sequence.

### Bracket Handling Detail

There's a small issue here: you may notice that the action for brackets doesn't write `emit`, but TOKENs are still added normally. Looking at ANTLR's source code:

```cpp
do {
    match(...);
} while (type == MORE);
if (token == nullptr)
    emit();
return std::move(token);
```

Here `emit()` is the default behavior, which sets `token` to the currently matched TOKEN. The code to execute the action is in the `match` function. If a manual emit is performed in the action, the default behavior won't occur. This also requires that we must set the member `token` to non-null in the overridden `emit` function.

### nextToken Function

The `nextToken` function has two main purposes:

1. **Return tokens in sequence**: It must return the contents of `tokens` in order
2. **Handle file end**: When encountering the end-of-file marker, since we want `suite` to end with `DEDENT`, we need to manually `emit` some `DEDENT` tokens

Another point to note is that the base class's `nextToken` function is called directly here for convenience. The function's return value is a `unique_ptr`, and the content it stores is already in `tokens`. Therefore, if we don't do any processing, premature destruction may occur.

(This assumes that the content stored in `token` is always a real and valid address. I considered changing it to directly pass in a useless address like `0x0001` during `emit`. This might make the feature clearer, and even if there's a Runtime Error due to some access, it would be easy to know it's a problem here.)

## Outstanding Issues

Memory management is not implemented for `tokens`.
