# Simplified Python Grammar Specification

## I. Program Structure

The program executes line by line according to control flow.

## II. Grammar Rules

### 1. Character Set

ASCII encoding, case-sensitive. Chinese characters are **undefined**. Legal character sets are as follows:

- **Identifiers** (including variable and function identifiers): 26 lowercase English letters, 26 uppercase English letters, digits `0` to `9`, underscore `_`
- **Standard Operators**: plus `+`, minus `-`, multiply `*`, float division `/`, integer division `//`, modulo `%`
- **Relational Operators**: greater than `>`, less than `<`, greater than or equal `>=`, less than or equal `<=`, not equal `!=`, equal `==`
- **Augmented Assignment Operators**: addition `+=`, subtraction `-=`, multiplication `*=`, float division `/=`, integer division `//=`, modulo `%=`
- **Assignment Operator**: assign `=`
- **Subscript Operator**: subscript access `[]`
- **Precedence Operator**: parentheses `()`
- **Separator**: comma `,`
- **Special Symbols**: space ` `, newline `\n`, tab `\t`, comment identifier `#`, string identifier `"` or `'`

### 2. Keywords

```python
None
True
False
def
return
break
continue
if
elif
else
while
or
and
not
```

Keywords cannot be used as variable names or function names.

### 3. Whitespace Characters

**Spaces** and **tabs** in source files can distinguish tokens and can also indicate indentation at the beginning of a line.

**Newlines** indicate the start of a new statement.

Indentation in Python is used to identify code blocks. Related issues will be handled by the Parser using ANTLR4, so you don't need to consider the related implementation.

### 4. Comments

Content from `#` to the end of the line is treated as a comment.

### 5. Identifiers

The first character of an identifier must be an English letter. Starting from the second character, it can be an English letter, digit, or underscore.

Identifiers are case-sensitive. Identifiers longer than 64 characters are **undefined**.

### 6. Constants

- **Boolean Constants**: `True` is true, `False` is false

- **Integer Constants**:
  - Integer constants are represented in decimal; non-decimal representation is **undefined** behavior
  - Integer constants do not have negative numbers; negative numbers can be obtained by negating positive numbers
  - In Python, the range of integers is unlimited. The same applies here, which means you must implement arbitrary precision integers
  - Integer constants starting with 0 are **undefined**

- **Float Constants**:
  - Float constants are represented in decimal
  - Float constants do not have negative numbers; negative numbers can be obtained by negating positive numbers
  - Floats with leading zeros are **undefined**, such as `0001.1`
  - Floats containing `e` are **undefined**, such as `1e5`
  - Floats starting with `.` are **undefined**, such as `.123`

- **String Constants**:
  - String constants are strings enclosed by double quotes `"` or single quotes `'`
  - Two strings can be concatenated to form a new string constant. For example, `"123""456"` is equivalent to `"123456"`
  - All characters in a string must be printable characters or spaces

- **Null Constant**: `None` is used to indicate that a variable does not point to any value

### 7. Data Types

- `bool`: Only `True` and `False`
- `int`: Arbitrary precision integer
- `float`: Consistent with `double` in C++
- `str`: String, **immutable!**
- Tuple: Specific behavior please refer to Python (to be completed)

### 8. Expressions

#### 8.1. Basic Expressions

Basic expressions include standalone constants, variables, function calls, and array access.

#### 8.2. Arithmetic Expressions

Legal arithmetic operators include: `+`, `-`, `*`, `/`, `//`, `%`. The specific behavior can refer to C++, with special behaviors as follows:

- **String Operators**:
  - `str + str` represents string concatenation
  - `str * int` represents string repetition
  - `str <= str` represents string comparison, comparison rules are the same as C++ `std::string`, `>=`, `==` etc. are similar

- **Division**:
  - `/` represents float division, i.e., the result is a float
  - `//` represents integer division, i.e., the result is an integer. Note that **floor division** is used regardless of sign, for example `-5 // 3 = -2`
  - `%` represents modulo operation, regardless of the sign of the divisor, it is defined as: `a % b = a - (a // b) * b`

- **Comparison**:
  - `==` and `!=` will try to convert both sides to the same type before comparison, but will not convert to `str` type. If conversion is not possible, the comparison result is `False`.

- **Chained Comparison**:
  - There are continuous relational operators like `1<2>3`. The processing method is to split them into adjacent comparisons and connect them with `and`, but **each value is evaluated at most once**
  - For example, `a()<b()<c()` is equivalent to first translating to `a()<b() and b()<c()` and then evaluating, where `a()`, `b()`, `c()` are called at most once (due to short-circuit operators)

- **Logical Operators**:
  - `and` is equivalent to `&&` in C++, `or` is equivalent to `||` in C++, `not` is equivalent to `!` in C++
  - `and` and `or` are **short-circuit** operators

- **Implicit Conversion**:
  - When the types of two operands are inconsistent during operation, they need to be converted to the same type before operation
  - Specific conversion rules refer to C++

Behaviors not included in special behaviors and C++ standards are **undefined** behaviors.

#### 8.3. Assignment Expressions

- Assigning a value to a variable means making this variable point to the right value, regardless of the right value's type
- For example, `a=1, a="123", a=1.1` executed sequentially, then output `a`, the result is `1.1`. Variable shadowing is allowed
- Multiple variables can be assigned, for example `a,b=1,2` means `a=1,b=2` executed sequentially. This may be **different** from standard Python
- Augmented assignment is a simplified expression of ordinary arithmetic operations. Specific behavior refers to C++

#### 8.4. Operator Precedence

From low to high (operators on the same line have the same precedence):

```python
=
or
and
not
< > <= >= == !=
+ -
* / // %
()
```

#### 8.5. Formatted Strings

An area enclosed by `f"` and `"` is called a "formatted string". In a formatted string, expressions enclosed by `{}` should be output as their values, for example:

```python
x = 1
y = 2
print(f"The value of x + y is {x + y}")
```

Should output `The value of x + y is 3`.

Important notes:
- In formatted strings, the value of `bool` type expressions should be output as `True` or `False` (not 0 and 1).
- Formatted strings should support nesting, for example: `f"{f"{True}"} == True"` is legal and should output `True == True`.
- For actual `{` and `}` characters in the string, use `{{` and `}}` for escaping.
- Expression types allowed inside `{}` can only be: `bool`, `int`, `float`, `string`. It is guaranteed that tuples will not be formatted.
- When converting `float` to string, please keep 6 decimal places, i.e., in the form of `1.000000`.

### 9. Statements

#### 9.1. Variable Definition/Assignment Statements

- Single variable assignment: `var = value`
- Chained assignment: `var_1 = var_2 = ... = value`

Refer to [Assignment Expressions](#83-assignment-expressions).

#### 9.2. Expression Statements

Refer to [Expressions](#8-expressions).

#### 9.3. Conditional Statements

```python
if expression_1:
    # code block
elif expression_2:
    # code block
else:
    # code block
```

`elif` is equivalent to `else if`. `else` can be omitted.

#### 9.4. Loop Statements

```python
while expression:
    # code block
```

#### 9.5. Jump Statements

- Includes `return`, `break`, and `continue`
- Specific behavior refers to C++

### 10. Functions

#### 10.1. Function Definition

```python
def func(parameters):
    # code block
```

- Parameter list like `a, b, c`, variable names separated by commas, can be empty
- Some variables can have default values, but they must all appear after variables without default values
- Functions can have return values or no return values, no need to declare explicitly. Return values can be multiple variables, like `return a, b`

#### 10.2. Function Calls

```python
func(parameters)
```

- Function calls must appear after the function definition.

- There are two forms of parameters: keyword and positional

  - **Keyword argument**: for example, `foo(a=1,b=2)` means passing parameter `a` with value `1`, `b` with value `2`
  - **Positional argument**: refers to `foo(1,2)` which assigns parameters in order of appearance
  - If a parameter list has both forms, positional arguments must appear before keyword arguments. For example, `foo(1,b=2)`

- The data guarantees that recursion depth does not exceed 2000 levels

### 11. Built-in Functions

- `print`: Output, can have any number of parameters, output one by one, separated by spaces. Output then newline. Output `float` with 6 decimal places. Output strings without quotes. For example, `print("123",1.0)` should output `123 1.000000`.

- `int`: Convert `float` or `bool` or `str` to `int`.

- `float`: Convert `int` or `bool` or `str` to `float`.

- `str`: Convert `int` or `float` or `bool` to `str`.

- `bool`: Convert `int` or `float` or `str` to `bool`. For `str`, if it is `""` then it is `False`, otherwise it is `True`.

- Type conversion functions all have only one parameter.

### 12. Scope

Scope-related rules:

- **Unlike standard Python, global variables are effective in all scopes** (can be accessed without the `global` keyword)

- **Only function calls create new scopes**. Scope creation follows Python rules

- There is no block-level scope. Variable deallocation only occurs when exiting function-level scope

- Global scope can be accessed without `global`. Therefore, the only way for local variables to override global variables is through the function parameter list.
