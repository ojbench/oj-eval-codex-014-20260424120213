# ANTLR in Python Interpreter

## Configuration Guide

### Configuring ANTLR C++ Runtime Environment

The Python interpreter uses ANTLR as the front-end parser. Since the core code takes a long time to compile, we have pre-compiled the ANTLR runtime environment so that your program does not need to compile the ANTLR runtime during compilation.

To use the ANTLR runtime environment on your own computer, you need to install the pre-compiled ANTLR runtime on your machine.

The `antlr4-runtime_4.13.1_amd64.deb` file is located in the `resources` directory. Navigate to the resources directory and execute the following command to install:

```shell
sudo apt install ./antlr4-runtime_4.13.1_amd64.deb
```

This package contains the ANTLR 4.13.1 dynamic library, static library, and header files. If you don't install this package, your program will fail to compile because it cannot find ANTLR's header files and dynamic libraries.
