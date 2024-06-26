# Stacky - A simple stack language
## Language Reference
### Version 0.1

## Disclaimer

The Stacky language is still in beta version 0.1, so things are moving fast and all versions below 1.0 may break **backwards compatability without notice**!

**USE THIS LANGUAGE AT YOUR OWN RISK!**

**IT SHOULD NOT BE CONSIDERED FOR PRODUCTION DEPLOYMENTS UNTIL FURTHER NOTICE!!!**

## Notational conventions

### Stack operations

The convention for showing how an command handles the stack is as follows:

```
<operation> : <stack before command> ---> <stack after command>
```
Examples:

```
+    : [ x y <] ---> [ (x+y) <]

swap : [ x y <] ---> [ y x <]

dup  : [ x <]   ---> [ x x <]

drop : [ x <]   ---> [ <]

```

**NOTE:** When a number of variables are shown on the stack, it denotes that *at least* that number of elements are present on the stack at that time. There may be more, but no less! Attempting to apply an operation with less then necessary elements on the stack will lead to a run-time error!

When the type of the operation is important, and it is not clear from the context, the variables can be denoted with a type:

```
+ : [ (x:integer) (y:integer) <] ---> [ (x+y:integer) <]
```

## Execution environment

### The stack

Stacky has an unbounded stack, which - in the reference implementation - is limited only by the available memory. Other implementations may impose some other limit inherent in that implementation, such as only allowing the total size of the stack to be less than what can be counted by an unsigned, 64 bit integer.

All items in in the language - with a few exceptions, operate solely on the stack and only the stack. Typical exceptions to that rule are I/O and variable operations.

### Variables and the environment

Stacky does not have variables of the kind one would find in other languages, such as C++, Go, or Java. It does however offer the possibility to bind a name to a value on the stack. This mapping of names to values is called the environment.

Any *atom* may be bound to a value by the *stash* (`;`) command:

```
; : [ v (n:atom) <] --- [ <]
```

This makes it possible to define, e.g., user defined commands.

```
` Compute the square of an integer
[ dup * ] sq;
```

**NOTE:** Attempting to bind an atom more than once will lead to a run-time error!

```
> 42 theAnswer;
[  <]
> theAnswer
[ 42 <]
> 33 theAnswer;
ERROR: Operation ';' expects an atom as key for, got '42 : integer'
> 33 'theAnswer;
ERROR: Redefining name: 'theAnswer'
```

The first error arises from the fact that `theAnswer` is bound, so it will be replaced by the value 42 before the stash (`;`) command is executed.

Putting a single quote (`'`) in front of an atom, will inhibit its evaluation. This is actually useful, when writing complex programs. 

**NOTE:** It is considered best practice to *always* prefix a name with the quote when storing it in the environment. Thus, the correct code for storing `theAnswer` is:

```
42 'theAnswer;
```

## Comments

Stacky has two kinds of comments. *Short comments* encompass everything between a backtick (`) and the end of the line.

*Long comments* on the other hand can encompass severa lines. They start and end with three backticks (\`\`\`).

Unlike most languages, Stacky consider all text to be comments until a long comment appears. This makes it possible to embed valid Stacky code in another format, such as [Markdown](https://en.wikipedia.org/wiki/Markdown).

Example:

~~~
This is the beginning of the file. It is a comment.
Below is the code.
```
"This is the code" 'And this is a short comment.
~~~

    
## Datatypes

The Stacky language has the following datatypes:

* Integers of unbounded size
* Atoms (also called names when appropriate).
* Strings
* Stacks

### Integers

The Stacky language does not put any limitations to the maximum size of an integer. The reference implementation uses a *BigInt* type that allows integers to grow to any size as long as it fits in memory. However, other implementations may put further limitations to the size of an integer, such as a limited bit size.

The following operations are defined on integers:

* [Arithmetic](#arithmetic-operators): `+`, `-`, `*`, `/`

* [Comparison](#comparison-operators): `=`, `<>`, `<`, `>`, `<=`, `>=`

All operations follow this pattern:
```
[ x y <] ---> [ (x 'op' y) <]
```

### Boolean values

Stacky does not define a *boolean* type, but uses the concept of "truthiness", i.e., it uses some values as false, and all other as true:

| Value | Comment                        |
|:-----:|:-------------------------------|
| `0`   | The integer zero (0).          |
| `""`  | The empty string.              |
| `[]`  | The empty [sub-stack](#stacks) |

The language defines the following boolean operations: `and`, `or`, `~` (*not*)

**NOTE:** Although the boolean operations accept several types of values as input, they only emot either `0` for false and `1` for true values.

Examples:

```
0 0 or           --->   0
0 1 or           --->   1

"" "Hi" and      --->   0
"Hello" 43 and   --->   1

[] not           --->   1

```

### Atoms

Atoms are short alphanumeric strings that mostly fill the role of identifiers in other languages. However, in Stacky, atoms are values unto their own. They can be compared and manipulated by operations.

An atom always starts with a letter, either upper or lower case. Ith then continues with letters, numbers and underscores. The valid atoms can be described with the following [regex](https://en.wikipedia.org/wiki/Regular_expression):

```
[a-zA-Z][a-zA-Z0-9_]*
```

The name atom in programming languages originated with [Prolog](https://en.wikipedia.org/wiki/Prolog) and Stacky uses them more or less as they are used in the [Erlang](https://en.wikipedia.org/wiki/Erlang_(programming_language)) programming language.

The most important operations that use atom are, inhibitor (`'`), stash (`;`) and evaluation.

When an atom is encountered during evaluation, one of two things can happen. If the previous operation was the inhibitor (`'`), then the atom will be pushed onto the stack. Without the inhibitor, the atom will be pushed onto the stack, unless it has been previously stored. If that is the case, the contents that was stored away will be evalutated.

Example:

```
19700101 epoch;      `First year of the UNIX epoch is stored.

epoch                'Since epoch is defined, 19700101 is pushed onto the stack.

'epoch               `The inhibitor ensures that the atom is pushed onto the stack.
```

**NOTE:** Best practice is to always inhibit an atom if it is going to be used as a value. Even if it isn't defined for the moment, it might become so in the future of the program's eceution.

| Operation  | Syntax | Stack                            | Comment            |
|:-----------|:------:|:---------------------------------|--------------------|
| Inhibitor  | `'`    | `[ ' a:atom <] ---> [ a <]`      |                    |
| Stash      | `;`    | `[ y a:atom ; <] ---> [  <]`     |                    |
| Evaluation | n/a    | `[ a:atom <]   ---> [ env(a) <]` | if `a` is defined. |
|            |        | `[ a:atom <]   ---> [  <]`       | otherwise.         |

### Strings

Strings are ordered sequences of 8-bit ASCII characters of arbitrary length enclosed in quotation marks (`"`). They may be empty.

Some unprintable characters are represented by *[escape sequences](https://en.wikipedia.org/wiki/Escape_sequence)*:

| Escape sequence | Character                                                    |
|:---------------:|:-------------------------------------------------------------|
| \\"             | A quotation mark (`"`) to prevent a end-of-string detection. |
| \\n             | Newline                                                      |
| \\r             | Carriage return                                              |
| \\t             | Horizontal tab                                               |
| \\\\            | A backslash {`\`)                                            |

The following operations mainly operate on strings:

| Operation | Syntax | Stack                                                    |
|:----------|:------:|:---------------------------------------------------------|
| Append    | `++`   | `[ s1:string s2:string <] ---> [ "s1 followed by s2" <]` |

### Stacks

Stacks are ordered lists of Stacky values. This is the main [composite datatype](https://en.wikipedia.org/wiki/Composite_data_type). The elements of a stack can be any Stacky value, such as atoms and integers, but also other stacks. Unlike other code, stacks aren't immediatelly evaluated, making it possible to delay execution and even store them in the environment. This is how new operations are defined in Stacky.

A stored stack will be immediatelly evaluated when it is retrieved.

Example:

```
[  <]
> [dup *]'sq;
[  <]
> 25 sq
[ 625 <]
```

The apply operation (`@`) is used to evaluate a stack.

Example:

```
[  <]
> [dup *]'sq;
[  <]
> [ 25 sq sq ]
[ [ 25 sq sq ] <]
> @
[ 390625 <]
```

The conditional operation (`?`) is basically an if - then - else construct. It takes three values on the stack. The third topmost is a predicate and when evaluated, its truthiness will decide which of the other topmost element to be evaluated. If true, the second topmost is selected, otherwise the topmost.

```
? : [ <predicate> <then-part> <else-part ? <]
    -->
    [ <then-part if predicate evaluates to to true, otherwise else-part> <]
```

Example:

```
[  <]
> 25   [50 >] ["OLD"] ["YOUNG"] ?
[ "YOUNG" <]
> 75   [50 >] ["OLD"] ["YOUNG"] ?
[ "YOUNG" "OLD" <]
> 
```

Finally, append (`++`) also works on stacks:

```
[  <]
> [ 1 2 3 ]
[ [ 1 2 3 ] <]
> [ 4 5 6 ]
[ [ 1 2 3 ] [ 4 5 6 ] <]
> ++
[ [ 1 2 3 4 5 6 ] <]
```

## Operation Reference

### Arithmetic operations

Arithmetic operators are only defined for *integers*. Any other argument type will lead to a run-time error'

| Operation | Stack                      | Comment          |
|:---------:|:---------------------------|:-----------------|
| `+`       | `[ x y <] ---> [ (x+y) <]` | Addition         |
| `-`       | `[ x y <] ---> [ (x-y) <]` | Subtraction      |
| `*`       | `[ x y <] ---> [ (xy) <]`  | Multiplication   |
| `/`       | `[ x y <] ---> [ (x/y) <]` | Integer division |

### Comparison operations

It is possible to compare all builtin datatypes. However, both arguments must be of the same type. Different types will never return a true values, e.g., comparing an integer and string will always return a false value.

| Operation | Stack                       | Comment                         |
|:---------:|:----------------------------|:--------------------------------|
| `=`       | `[ x y <] ---> [ (x=y) <]`  | x is equal to y                 |
| `<>`      | `[ x y <] ---> [ (x<>y) <]` | x is not equal to y             |
| `<`       | `[ x y <] ---> [ (x<y) <]`  | x is less than y                |
| `>`       | `[ x y <] ---> [ (x>y) <]`  | x is greater than y             |
| `<=`      | `[ x y <] ---> [ (x<=y) <]` | x is less than or equal to y    |
| `>=`      | `[ x y <] ---> [ (x>=y) <]` | x is greater than or equal to y |

### Boolean operations

As described [above](#boolean-values), Stacky does not provide a boolean type. Instead it has a notion of *[truthiness](#boolean-values)*, i.e, some values represent the truth value *false*, while all other values represent *true*.

While the boolean operator accept several values, they will all return zero (`0`) for *false*, and one (`1`) for *true*.

| Operation | Stack                         | Comment |
|:---------:|:------------------------------|:--------|
| `and`     | `[ x y <] ---> [ and(x,y) <]` | And     |
| `or`      | `[ x y <] ---> [ or(x,y) <]`  | Or      |
| `~`       | `[ x <]   ---> [ not(x) <]`   | Not     |

### Control operations

Control opertions are operations that either control the flow or execution or affects the execution environment.

#### The stash operation

The *stash* (`;`) operation's single purpose is to bind names to the *Name -> Value* map in the environment. It takes values and stashes them in the environment, thus the name.

The names that are created are immediatelly made available to the program.

**NOTE:** Bound names can not be updated! It is similar to how names (variables) are treated in mathematics and in functional programming languages, like Haskell or Erlang.

| Operation | Stack                             | Comment                    |
|:---------:|:----------------------------------|:---------------------------|
| `;`       | `[ value name:atom <] ---> [  <]` | *name* is bound to *value. |

Examples:

```
42 'theAnswer;               ` The value 42 is bound to theAnswer

[ dup * ] 'square            ` This is how functions are defined.
                             ' Store a sub-stack!

theAnswer square             ' Compute 42 * 42 = 1764
```
#### The `cond` operation

The *cond* operation (`?`) is the way one controls the execution flow in Stacky. It is basically the same as the *if-then-else* construct found in many languages. Its syntax and semantics are as follows:

| Operation | Stack                                | Comment                       |
|:---------:|:-------------------------------------|:------------------------------|
| `?`       | `[ pred then else <] ---> [ then <]` | If `pred` evaluates to *true* |
|           | `[ pred then else <] ---> [ else <]` | Otherwise                     |

In another language one might write something like this to compute a discount:

```
compute_discount(age, price) =
    if age >= 65 then
        return (price / 2)
    else
        return price
```

In stacky, this becomes:

```
[ 'price; 'age;
  [ age 65 >= ]
      [ price 2 / ]
      [ price ]
      ?
] 'compute_discount;

[  <]
> 20 100 compute_discount print
100
[  <]
> 75 100 compute_discount print
50
[  <]
```

(`print` takes a term from the stack and prints it on *stdout*.)

So the `if <predicate> then <do-if-true> else <do-if-false>` becomes `<predicate> <do-if-true> <do-if-false> ?`.

**NOTE:** the sub-stacks are not necessary unless a branch needs to execute more than one operation. The example above can be rewritten as:

```
[ swap 65 >= 2 1 ? / ] 'compute_discount;

```

More concise, but the readability may be affected...

### Stack operations

TBD

### String operations

TBD

### Sub-stack operations

TBD

### Input/Output operations

TBD

### Reflection/introspection operations

TBD

## Syntax description

```
Commands         <-- Command*

Command          <-- Integer
                 <-- Atom
                 <-- String
                 <-- Stack

Stack            <-- '[' Commands ']'

Integer          <-- [-]?[0-9]+

Atom             <-- [a-zA-Z][a-zA-Z0-9_]*
                 <-- Builtin-Operator

String           <-- '"' Character* '"'

Builtin-Operator <-- '+' | '-' | '*' | '/' | '=' | '<>' | '<' | '>' | '<=' | '>=' | '~' 

Character        <-- ASCII-Character
                 <-- Escape-Sequence

Escape-Sequence  <-- '\'[\"rnt]
```
