# Python Interpreter Assignment

**ACMOJ Problem ID**: 2515

## Table of Contents

- [Python Interpreter Assignment](#python-interpreter-assignment)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
    - [Background](#background)
  - [Assignment Description](#assignment-description)
    - [Grade Composition](#grade-composition)
  - [Assignment Requirements](#assignment-requirements)
    - [Implementation Requirements](#implementation-requirements)
    - [Evaluation Method](#evaluation-method)
      - [Test Point Distribution](#test-point-distribution)
  - [Test Data](#test-data)
  - [Implementation Guidelines](#implementation-guidelines)
    - [Repository Structure](#repository-structure)
    - [Grammar Specification](#grammar-specification)
    - [ANTLR Setup](#antlr-setup)
    - [Implementation Workflow](#implementation-workflow)
    - [Code Navigation](#code-navigation)
  - [Submission Requirements](#submission-requirements)
    - [OJ Git Repository Compilation Process](#oj-git-repository-compilation-process)
    - [Submission Guidelines](#submission-guidelines)
    - [Evaluation Notes](#evaluation-notes)
    - [Academic Integrity](#academic-integrity)

## Introduction

### Background

In this assignment, you are required to implement a simple Python interpreter that accepts simplified Python code and executes it according to control flow.

> An interpreter is a computer program that can directly execute instructions written in an interpreted language.
> The interpreter acts like a "middleman". It interprets and executes code line by line, so programs dependent on interpreters run relatively slowly.
> The advantage of an interpreter is that it doesn't need to recompile the entire program, reducing the compilation burden after each program update.
> In contrast, a compiler translates all source code into binary files at once, and execution doesn't depend on the compiler or other additional programs.

## Assignment Description

### Grade Composition

| Component | Percentage |
| :--: | :--: |
| OJ Score | 80% |
| Code Review | 20% |

- Local sample data are provided to help with development and debugging. Passing local samples does not guarantee passing OJ tests.

- In the Code Review, we will **strictly examine your code style and repository organization structure, etc.**

## Assignment Requirements

### Implementation Requirements

1. Use OOP (Object-Oriented Programming) to implement the Python Interpreter.
2. Some test data are distributed in the assignment repository, while other data are not provided.

### Evaluation Method

Use `git` to submit on the OJ. The OJ will build your program based on the `CMakeLists.txt` file in the root directory.

**Per-Testcase Resource Limits:**
- **Time Limit (per test case)**: 500 ms (min), 16000 ms (max)
- **Memory Limit (per test case)**: 512 MiB
- **Disk Usage**: Disk access is not permitted

#### Test Point Distribution

Test points are distributed as follows:

```text
BigIntegerTest: 1 - 20 
Sample: 21 - 34 
AdvancedTest: 35 - 52 
ComplexTest: 53 - 56 
CornerTest: 57 - 66 
```

**Note:** Test cases in `./testcases` are provided, but they may not be identical to those on the OJ.

## Test Data

Public test cases for local testing are provided at:
- `./testcases/basic-testcases/` - Basic test cases (test0-test15)
- `./testcases/bigint-testcases/` - Big integer test cases (BigIntegerTest0-BigIntegerTest19)

Each test file contains:
- Input Python code (`.in` file)
- Expected output based on Python semantics (`.out` file)

## Implementation Guidelines

### Repository Structure

```
├── CMakeLists.txt
├── README.md
├── docs/
│   ├── grammar.md          # Python grammar specification
│   ├── antlr_guide.md      # ANTLR installation and usage guide
│   └── workflow_details.md # Implementation workflow details
├── generated/              # ANTLR-generated parser files
│   ├── CMakeLists.txt
│   ├── Python3Lexer.cpp
│   ├── Python3Lexer.h
│   ├── Python3Parser.cpp
│   ├── Python3Parser.h
│   ├── Python3ParserBaseVisitor.cpp
│   ├── Python3ParserBaseVisitor.h
│   ├── Python3ParserVisitor.cpp
│   └── Python3ParserVisitor.h
├── resources/              # ANTLR resources and grammar files
│   ├── antlr-python-tool-linux-amd64.deb
│   ├── antlr4-runtime_4.13.1_amd64.deb
│   ├── gen.sh
│   ├── Python3Demo.g4
│   ├── Python3Lexer.g4
│   └── Python3Parser.g4
├── src/                    # Your implementation files
│   ├── Evalvisitor.cpp
│   ├── Evalvisitor.h       # Main visitor implementation (TODO)
│   └── main.cpp
├── submit_acmoj/
│   └── acmoj_client.py
└── testcases/
    ├── basic-testcases/
    └── bigint-testcases/
```

### Grammar Specification

For the complete Python grammar specification used in this assignment, please refer to [Grammar Documentation](docs/grammar.md).

Key features include:
- **Data Types**: `bool`, `int` (arbitrary precision), `float` (double precision), `str`, and tuples
- **Operators**: Arithmetic (`+`, `-`, `*`, `/`, `//`, `%`), Comparison (`>`, `<`, `>=`, `<=`, `==`, `!=`), Logical (`and`, `or`, `not`)
- **Control Flow**: `if-elif-else`, `while`, `break`, `continue`
- **Functions**: Function definition with `def`, parameters with default values, keyword and positional arguments
- **Built-in Functions**: `print`, `int`, `float`, `str`, `bool`
- **Special Features**: F-strings (formatted strings), multiple assignment, operator precedence

### ANTLR Setup

For ANTLR installation and setup instructions, see [ANTLR Guide](docs/antlr_guide.md).

**Quick Setup:**

1. Install ANTLR runtime:
```bash
sudo apt install ./resources/antlr4-runtime_4.13.1_amd64.deb
```

This package contains ANTLR 4.13.1 dynamic libraries, static libraries, and header files required for compilation.

### Implementation Workflow

If you want to know where to start, refer to the [Workflow Details](docs/workflow_details.md).

### Code Navigation

For details on how to traverse the parse tree, refer to: [Step 4 in Workflow Details](docs/workflow_details.md#step-4-complete-srcevalvisitorh).

**Important Implementation Notes:**

- **Arbitrary Precision Integers**: Python integers have no size limit. You must implement big integer arithmetic.
- **Float Precision**: Output floats with 6 decimal places (e.g., `1.000000`).
- **F-String Support**: Implement formatted string literals with expression evaluation inside `{}`.
- **Scope Rules**: Unlike standard Python, global variables are accessible everywhere without the `global` keyword.
- **Type Conversion**: Implement implicit and explicit type conversions between `int`, `float`, `bool`, and `str`.
- **Memory Management**: Pay attention to memory leaks.

## Submission Requirements

### OJ Git Repository Compilation Process

For Git compilation, we will first clone the repository using a command similar to:
```bash
git clone <repo_url> . --depth 1 --recurse-submodules --shallow-submodules --no-local
```

Then we check if there is a `CMakeLists.txt` file. If it exists, we run (if not, a warning message will be displayed):
```bash
cmake .
```

Finally, we check if there is any of `GNUmakefile`/`makefile`/`Makefile` (if cmake was run previously, this will be the generated Makefile). If it exists, we run (if not, a warning message will be displayed):
```bash
make
```

After this process is complete, we will use the `code` file in the project root directory as the compilation result.

A `CMakeLists.txt` file is provided in the project. You can use or modify it as needed. The local environment has gcc-13 and g++-13 available.

### Submission Guidelines

- Submit via git repository URL on ACMOJ
- Repository format: `https://github.com/Your_Username/Your_Repo`
- **Executable name must be `code`** (check CMakeLists.txt)
- Program must read from standard input and write to standard output
- Ensure your implementation meets time and memory limits
- Use C++ with the provided ANTLR framework

**IMPORTANT**: Create a `.gitignore` file in your project root directory to avoid OJ evaluation conflicts.

The `.gitignore` file should include at least:
```gitignore
CMakeFiles/
CMakeCache.txt
```

### Evaluation Notes

- The evaluator will test your program with 66 test cases distributed across different difficulty levels
- Output must match the reference implementation exactly
- Exceeding time or memory limits will result in TLE/MLE verdict
- Focus on correctness, especially for:
  - Arbitrary precision integer arithmetic
  - Proper operator precedence
  - Correct scope handling
  - F-string formatting
  - Type conversions

---

**For complete grammar specifications and detailed implementation guidance, please refer to the documentation in the `docs/` directory.**

### Academic Integrity

If any violations are found during evaluation or code review (including but not limited to using unconventional methods to pass test cases), your final score may be significantly reduced or become **0 points**.
