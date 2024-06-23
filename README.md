<center>
# Stacky - A simple stack language
## Language Reference
### Version 0.1

## Disclaimer

The Stacky language is still in beta version 0.1, so things are moving fast and all versions below 1.0 may break **backwards compatability without notice**!

**USE THIS LANGUAGE AT YOUR OWN RISK!**

**IT SHOULD NOT BE CONSIDERED FOR PRODUCTION DEPLOYMENTS UNTIL FURTHER NOTICE!!!**
</center>

## Introduction

*Stacky* is a simple stack-based language. This means that all operations use a [stack](https://en.wikipedia.org/wiki/Stack_(abstract_data_type)) to get parameters from and convey results to.

In short, this means that most elements of the language are commands that operate on the stack. Even literals such as `42` are commands in Stacky. `42` is a command that pushes the integer value 42 onto the stack.

## Hello World

One of the simplest programs used to show a language is the so called *Hello World* program, which simply prints the greeting on the terminal and then terminates.

This is *Hello World* in Stacky:

```
"Hello World" putLn
```

It consists of the string literal `"Hello World"` that pushes the string onto the stack and `putLn` that takes the value on the top of the stack and prints it.

## The Stack

The [stack](https://en.wikipedia.org/wiki/Stack_(abstract_data_type)) is a common data structure in Computer Science. It's an ordered collection of values with two operations:

* `push` - Takes a value and inserts it into the stack. The values are stored in the order they are entered into the stack.
* `pop` - Retrieves the most recently pushed elemen from the stack. It is removed from the stack, as well.

In Stacky, the stack is represented as a sequence between `[` and `<]`. The stack is listed from bottom on the left to the top on the right hand side. When we start the Stacky interpreter, we are presented with the (initially) empty stack and a prompt where we enter our commands:

```
$ stacky 

STACKY version: 0.1, build: v2024-06-23.12-13-49

Copyright (c) 2024 Bengt Johansson -- All rights reserved

[  <]
> 
```

Let's enter the *Hello World* program in stages. First, we enter the string:

```
> "Hello World"
[ "Hello World" <]
> 
```

After doing so, we now see that the string has been pushed onto the stack as the stack printout now shows `[ "Hello World" <]`.

Now we enter the `putLn` command:

```
> putLn
Hello World
[  <]
> 
```

This takes the greeting from the stack, prints it and returns. As can be seen, the stack is now empty.

Here is another example of how the stack works, using the `+` command.

```
> 1 2 3 4 5 6
[ 1 2 3 4 5 6 <]
> +
[ 1 2 3 4 11 <]
> +
[ 1 2 3 15 <]
> +
[ 1 2 18 <]
> +
[ 1 20 <]
> +
[ 21 <]
> 
```

First, six integers are pushed onto the stack. Next the plus (`+`) command takes the two topmost elements, adds them together and pushes the result onto the stack once more.

In the rest of the manual, we will denote operations on the stack as follows:

```
+ : [ x y <] ---> [ (x+y) <]
```

**NOTE:** Unlike most ordinary languages, where function parameters can have varying arities, in most stck-based languages the results of an operation may have more than unary arity. Take for instance the swap operation, that swaps the frontmost elements on the stack. It is binary on the arguments, as well as the results.

```
swap: [ x y <] ---> [ y x <]
```

# Language reference

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

## Comments

Stacky has two kinds of comments. *Short comments* encompass everything between a backtick (`) and the end of the line.

*Long comments* on the other hand can encompass severa lines. They start and end with three backticks (\`\`\``).

Unlike most languages, Stacky consider all text to be comments until a long comment appears. This makes it possible to embed valid Stacky code in another format, such as [Markdown](https://en.wikipedia.org/wiki/Markdown).

Example:

~~~
This is the beginning of the file. It is a comment.
Below is the code.
```
"This is the code" 'And this is a short comment.
~~~

## The stack

Stacky has an unbounded stack, which - in the reference implementation - is limited only by the available memory. Other implementations may impose some other limit inherent in that implementation, such as only allowing the total size of the stack to be less than what can be counted by an unsigned, 64 bit integer.

All items in in the language - with a few exceptions, operate solely on the stack and only the stack. Typical exceptions to that rule are I/O and variable operations.

## Variables

Stacky does not have variables of the kind one would find in other languages, such as C++, Go, or Java. It does however offer the possibility to bind a name to a value on the stack. This makes it possible to define, e.g., user defined commands.

Any *atom* may be bound to a value by the *stash* (`;`) command:

```
; : [ v (n:atom) <] --- [ <]
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

**NOTE:** It is considered best practice to *always* prefix a name with the quote when stashing it. Thus, the correct code for storing `theAnswer` is:

```
42 'theAnswer;
```
    
## Datatypes

The Stacky language has the following datatypes:

* Integers of unbounded size
* Atoms (also called names when appropriate).
* Strings
* Stacks

### Integers

The Stacky language does not put any limitations to the maximum size of an integer. The reference implementation uses a *BigInt* type that allows integers to grow to any size as long as it fits in memory. However, other implementations may put further limitations to the size of an integer, such as a limited bit size.

The following operations are defined on integers:

* Arithmetic: 

  | Operation      | Syntax |
  |:---------------|:------:|
  | Addition       | `+`    |
  | Subtraction    | `-`    |
  | Multiplication | `*`    |
  | Division       | `/`    |

* Comparison:

  | Operation             | Syntax |
  |:----------------------|:------:|
  | Equal to              | `=`    |
  | Not equal to          | `<>`   |
  | Less than             | `<`    |
  | Greater than          | `>`    |
  | Less than or equal    | `<=`   |
  | Greater than or equal | `>=`   |
  
All operations follow this pattern: `[ x y <] ---> [ (x 'op' y) <]

### Boolean values

Stacky does not define a *boolean* type, but uses the concept of "truthiness", i.e., it uses some values as false, and all other as true:

| Value | Comment                        |
|:-----:|:-------------------------------|
| `0`   | The integer zero (0).          |
| `""`  | The empty string.              |
| `[]`  | The empty [sub-stack](#stacks) |

The language defines the following boolean operations:

| Operation             | Syntax | Stack                         |
|:----------------------|:------:|:------------------------------|
| And                   | `=`    | `[ x y <] ---> [ and(x,y) <]` |
| Or                    | `<>`   | `[ x y <] ---> [ or(x,y) <]`  |
| Not                   | `<`    | `[ x <]   ---> [ not(x) <]`   |

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

When an atom is encountered during evaluation, one of two things can happen. If the previous operation was the inhibitor (`'`), then the atom will be pushed onto the stack. Without the inhibitor, the atom will be pushed onto the stack, unless it has been previously stashed. If that is the case, the contents that was stashed away will be evalutated.

Example:

```
19700101 epoch;      `First year of the UNIX epoch is stashed.

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

Stacks are ordered lists of Stacky values. This is the main [composite datatype](https://en.wikipedia.org/wiki/Composite_data_type). The elements of a stack can be any Stacky value, such as atoms and integers, but also other stacks. Unlike other code, stacks aren't immediatelly evaluated, making it possible to delay execution and even store them using stashing This is how new operations are defined in Stacky.

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

TBD

### Arithmetic operations

TBD

### Comparison operations

TBD

### Boolean operations

TBD

### Control operation

TBD

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

