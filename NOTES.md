# Notes

This keeps track of some development notes.

## Installing cvc5-ff

If you are installing it for Docker, checkout `Dockerfile@base` in `resources/` folder.

If you use Mac, make sure that you have `brew` installed.

1. Clone [https://github.com/cvc5/cvc5](https://github.com/cvc5/cvc5). (Known to work with version `de62429`.)
2. Install `libgmp`. E.g., on Ubuntu, run `apt install libgmp-dev`. On Mac, run `brew install gmp`.
3. Run `pip3 install tomli scikit-build Cython`. If you use Mac, make sure that your Python is not the one installed by default with Mac. (Use `brew install python` -- known to work with Python 3.12.)
4. Run `./configure.sh --cocoa --auto-download --python-bindings`.
5. Go to the `build` subdirectory and run `make -j4 install`.

## The Circom DSL

Transcribed from [ast.rs](./circom-parser/program-structure/src/abstract_syntax_tree/ast.rs).

```
terminal: uint | int | string | bool | list<T> | symbol | null
setype: 'empty | 'binary | 'fieldelem
stype: 'output | 'input | 'intermediate
signal: (stype, setype)
vtype: 'var | 'comp | signal
version: (uint, uint, uint)
access: string | expression
assignop: 'var | 'sig | 'csig
infixop: 'mul | 'div | 'add | 'sub | 'pow | 'intdiv | 'mod | 'shl | 'shr
       | 'leq | 'geq | 'lt | 'gt | 'eq | 'neq | 'or | 'and | 'bor | 'band | 'bxor
prefixop: 'neg | 'not | 'comp
treduction: 'var | 'comp | 'sig

fileloc
  |- "start": uint
  |- "end": uint

number
  |- "meta": meta
  |- "v": int

tknowledge
  |- "rto": treduction | null

mknowledge
  |- "cdims": list<uint> | null
  |- "len": uint | null
  |- "absmaddr": uint | null

meta
  |- "elemid": uint
  |- "start": uint
  |- "end": uint
  |- "loc": fileloc
  |- "fileid": uint | null
  |- "compinf": string | null
  |- "tk": tknowledge
  |- "mk": mknowledge

circom
  |- "meta": meta
  |- "ver": version | null
  |- "incs": list<string>
  |- "defs": list<definition>
  |- "main": component | null
  
component: (list<string>, expression)

definition: template | function

template
  |- "meta": meta
  |- "name": string
  |- "args": list<string>
  |- "argloc": fileloc
  |- "body": statement
  |- "parallel": bool

function
  |- "meta": meta
  |- "name": string
  |- "args": list<string>
  |- "argloc": fileloc
  |- "body": statement

statement: itestmt | whilestmt | retstmt | declstmt | substmt | ceqstmt
         | logcallstmt | assertstmt | initblock | block

itestmt
  |- "meta": meta
  |- "cond": expression
  |- "if": statement
  |- "else": statement | null
  
whilestme
  |- "meta": meta
  |- "cond": expression
  |- "stmt": statement

retstmt
  |- "meta": meta
  |- "val": expression

declstmt
  |- "meta": meta
  |- "xtype": vtype
  |- "name": string
  |- "dims": list<expression>
  |- "constant": bool

substmt
  |- "meta": meta
  |- "var": string
  |- "access": list<access>
  |- "op": assignop
  |- "rhe": expression
;
ceqstmt
  |- "meta": meta
  |- "lhe": expression
  |- "rhe": expression

logcallstmt
  |- "meta": meta
  |- "arg": expression

assertstmt
  |- "meta": meta
  |- "arg": expressions

initblock
  |- "meta": meta
  |- "xtype": vtype
  |- "inits": list<statement>

block
  |- "meta": meta
  |- "stmts": list<statement>

expression: infix | prefix | inlineswitch | variable | number | call | arrayinline

infix
  |- "meta": meta
  |- "lhe": expression
  |- "op": infixop
  |- "rhe": expression

prefix
  |- "meta": meta
  |- "op": prefixop
  |- "rhe": expression

inlineswitch
  |- "meta": meta
  |- "cond": expression
  |- "true": expression
  |- "false": expression

variable
  |- "meta": meta
  |- "name": string
  |- "access": list<access>

call
  |- "meta": meta
  |- "id": string
  |- "args": list<expression>

arrayinline
  |- "meta": meta
  |- "vals": list<expression>
```

## The Tokamak Symbolic Programming Pattern (Draft)

<div align="left">Current Version Badge: <img src="https://img.shields.io/badge/tokamak-0.1-blueviolet?labelColor=blueviolet&color=3d3d3d"></div>

The tokamak pattern is used to restrict the scope of symbolic variables in a project, so as to make developing and debugging a project written using racket/rosette easier.

#### Overall Principle

- **T0: Whoever processes lifts.**
  - `for/all`
  - Symbolic variables should ***ONLY*** be lifted by those functions that process/interact with them.
  - This prevents performance drop by wrapping too many nested `for/all`'s from upper level calls.
- **T1: Whoever defines checks.**
  - `tokamak:typed`
  - A variable type should be checked/expected whenever it's defined, especially those defined by `for/all` construct.
  - This prevents unexpected/uncontrolled type flow.
  - If the type of a defined variable is known for sure or ensured by the function called (e.g., builtin racket/rosette function), the type checking can be skipped.
- **T2: Whoever tags overrides.**
  - If a function processes an argument and specifically tags it as `concrete` only, then it's the caller's responsibility to make sure of it (by lifting or whatever). In this case, this overrides T0.

#### Terms Used

- Symbolic Scope: A scope that may deal with symbolic variables.
- Symbolic Status: A symbolic status is tagged with each argument of the function that introduces a symbolic scope, indicating whether the argument is expected to be symbolic or not.

#### Naming Conventions

- A local variable in a `for/all` scope is usually named `<var><lvl>`, where `<var>` is the original name and `<lvl>` is the number of nested levels.
  - Example: `name0`, `rhe1`.

#### Tagging & Checking Symbolic Statuss

- A function that introduces a symbolic scope should explicitly tag and check the expected symbolic status of each of its arguments.

  - Usually the arguments tagged with `concrete` are then checked.

  - The arguments tagged with `symbolic` can be either symbolic or concrete.

  - Example

    ```scheme
    ; (concrete:top) arg-node
    ; (symbolic:top) arg-scopes
    ; (symbolic:top) arg-prefix
    (define (do-interpret arg-node arg-scopes arg-prefix)
      (tokamak:typed arg-node circom:lang?)
    )
    ```

#### Decomposability

(pending)

#### Approaching Indecomposable Variables

(pending)

#### Connecting Symbolic Variables

(pending)

