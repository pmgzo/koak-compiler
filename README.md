# Koak

This is an implementation of the koak compiler. Koak is a language close to kaleidoscope.
In this projects we build the parser, the AST, then we transform the AST to LLVM IR.
Then LLVM is handling the backend of the compilation.
Basically by runing the project we are able to build object file (**.o**), and we must link them with another compiler like **gcc**
To get the execution file.

## Koak file examples

You can find some koak files in the *./exampleKoak* repository

## Installations

Based on this link https://github.com/llvm-hs/llvm-hs/issues/231, we need to install llvm-9 to use haskell's binding with LLVM

### Fedora installation:
- install this package: llvm9.0-devel.x86_64
- add this in your PATH env variable: /usr/lib64/llvm9.0/
- ```stack build```

Also, You first need to install stack which is the haskell's package manager.

## Run project

``
make
``

### Run test

``
make tests_run
``

### Run e2e tests with .ko files

``
make tests_integrations
``
