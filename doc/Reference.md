# Stacky - A simple stack language
## Language Reference
### Version 0.1

## Disclaimer

The Stacky language is still in beta version 0.1, so things are moving fast and all versions below 1.0 may break **backwards compatibility without notice**!

**USE THIS LANGUAGE AT YOUR OWN RISK!**

**IT SHOULD NOT BE CONSIDERED FOR PRODUCTION DEPLOYMENTS UNTIL FURTHER NOTICE!!!**

## Notational conventions

### Stack operations

The convention for showing how an command handles the stack is as follows:

```
<operation> : <stack before command> ---> <stack after command>
```

This reads as `<operation>` transforms the stack looking like '<stack before command>' (the *input stack*) to '<stack after command>' (the *output stack*).

It is assumed that the number of elements on the input stack is the minimal number needed to perform the operation. So, `+`, see below, needs at least two elements on the stack and leaves one element on the stack as the result of the operation.

Sometimes, other conventions may be used to show *how* the stack is affected. Ellipsis (`...`) is used do denote zero or more elements as in `[x_n ... x_1 <]` denoting a stack with exactly *n* elements.

Examples:

```
+      : [ x y <]           ---> [ (x+y) <]
swap   : [ x y <]           ---> [ y x <]
depth  : [ x_n ... x_1 <]   ---> [ x_n ... x_1 n <]

```

**NOTE:** When a number of variables are shown on the stack, it denotes that *at least* that number of elements are present on the stack at that time. There may be more, but no less! Attempting to apply an operation with less then necessary elements on the stack will lead to a run-time error!

### Type notation

When the type of the operation is important, and it is not clear from the context, the variables can be denoted with a type:

```
+ : [ (x:integer) (y:integer) <] ---> [ (x+y:integer) <]
```

When the length of a variable type, such as a list or a string, is of importance, it is put in parenthesis after the type:

```
length [ xs:list(n) <] ---> [ n <]
```

This means that `xs` which is a list of *n* elements is replaced with the lenght.

Two elements in the parenthesis denotes a range. `list(1,7)` is a list of at least one element but less than seven. These forms are available:

| Form     | Meaning                                                        |
|:---------|:---------------------------------------------------------------|
| `T`      | Type *T* with any number of elements.                          |
| `T(n)`   | Type *T* with exactly *n* elements.                            |
| `T(m,n)` | Type *T* with at least *m* element and less than *n* elements. |
| `T(,n)`  | Type *T* zero (0) or more elements and less than *n* elements. |
| `T(m,)`  | Type *T* with at least *m* or more elements.                   |

Examples:

```
++ : [ xs:list(m) ys:list(n) <] ---> [ zx:list(m+n)
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

Stacky has two kinds of comments. *Short comments* encompass everything between a back-tick (`) and the end of the line.

*Long comments* on the other hand can encompass several lines. They start and end with three back-ticks (\`\`\`).

Unlike most languages, Stacky consider all text to be comments until a long comment appears. This makes it possible to embed valid Stacky code in another format, such as [Markdown](https://en.wikipedia.org/wiki/Markdown).

Example:

~~~
This is the beginning of the file. It is a comment.
Below is the code.
```
"This is the code" 'And this is a short comment.
~~~

    
## Data-types

The Stacky language has the following data-types:

* [Integers](#integers) of unbounded size
* [Atoms](#atoms) (also called names when appropriate).
* [Strings](#strings)
* [Lists](#lists)

### Integers

The Stacky language does not put any limitations to the maximum size of an integer. The reference implementation uses a *BigInt* type that allows integers to grow to any size as long as it fits in memory. However, other implementations may put further limitations to the size of an integer, such as a limited bit size.

The following operations are defined on integers:

* [Arithmetic](#arithmetic-operations): `+`, `-`, `*`, `/`, `%`

* [Comparison](#comparison-operations): `=`, `<>`, `<`, `>`, `<=`, `>=`

All operations follow this pattern:
```
[ x y <] ---> [ (x 'op' y) <]
```

### Boolean values

Stacky does not define a *boolean* type, but uses the concept of "truthiness", i.e., it uses some values as false, and all other as true:

| Value | Comment                  |
|:-----:|:-------------------------|
| `0`   | The integer zero (0).    |
| `""`  | The empty string.        |
| `[]`  | The empty [list](#lists) |

The language defines the following boolean operations: `and`, `or`, `~` (*not*)

**NOTE:** Although the boolean operations accept several types of values as input, they only emit either `0` for false and `1` for true values.

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

An atom always starts with a letter, either upper or lower case. It then continues with letters, numbers and underscores. The valid atoms can be described with the following [regex](https://en.wikipedia.org/wiki/Regular_expression):

```
[a-zA-Z][a-zA-Z0-9_]*
```

The name atom in programming languages originated with [Prolog](https://en.wikipedia.org/wiki/Prolog) and Stacky uses them more or less as they are used in the [Erlang](https://en.wikipedia.org/wiki/Erlang_(programming_language)) programming language.

The most important operations that use atom are, inhibitor (`'`), stash (`;`) and evaluation.

When an atom is encountered during evaluation, one of two things can happen. If the previous operation was the inhibitor (`'`), then the atom will be pushed onto the stack. Without the inhibitor, the atom will be pushed onto the stack, unless it has been previously stored. If that is the case, the contents that was stored away will be evaluated.

Example:

```
19700101 epoch;      `First year of the UNIX epoch is stored.

epoch                'Since epoch is defined, 19700101 is pushed onto
                     `the stack.

'epoch               `The inhibitor ensures that the atom is pushed onto
                     `the stack.
```

**NOTE:** Best practice is to always inhibit an atom if it is going to be used as a value. Even if it isn't defined for the moment, it might become so in the future of the program's execution.

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

| Operation | Syntax | Stack                                                         |
|:----------|:------:|:--------------------------------------------------------------|
| Append    | `++`   | `[ s1:string(m) s2:string(n) <] ---> [ "s1s2:string(m+n)" <]` |

### Lists

Lists are ordered sequences of Stacky values. This is the main [composite data-type](https://en.wikipedia.org/wiki/Composite_data_type). The elements of a list can be any Stacky value, such as atoms and integers, but also other lists. Unlike other code, lists aren't immediately evaluated, making it possible to delay execution and even store them in the environment. This is how new operations are defined in Stacky.

A stored list will be immediately evaluated when it is retrieved.

Example:

```
[  <]
> [dup *]'sq;
[  <]
> 25 sq
[ 625 <]
```

The apply operation (`@`) is used to evaluate a list.

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

Finally, append (`++`) also works on lists:

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

Arithmetic operators are only defined for *integers*. Any other argument type will lead to a run-time error'. If there are not enough elements on the stack for the operation, an error will occur.

| Operation | Stack                      | Comment                       |
|:---------:|:---------------------------|:------------------------------|
| `+`       | `[ x y <] ---> [ (x+y) <]` | Addition                      |
| `-`       | `[ x y <] ---> [ (x-y) <]` | Subtraction                   |
| `*`       | `[ x y <] ---> [ (xy) <]`  | Multiplication                |
| `/`       | `[ x y <] ---> [ (x/y) <]` | Integer division              |
| `%`       | `[ x y <] ---> [ (x/y) <]` | Integer reminder as in C(++). |

### Comparison operations

It is possible to compare all built-in data types. However, both arguments must be of the same type. Different types will never return a true values, e.g., comparing an integer and string will always return a false value.

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

Control operations are operations that either control the flow or execution or affects the execution environment.

#### The stash operation

The *stash* (`;`) operation's single purpose is to bind names to the *Name -> Value* map in the environment. It takes values and stashes them in the environment, thus the name.

The names that are created are immediately made available to the program.

**NOTE:** Bound names can not be updated! It is similar to how names (variables) are treated in mathematics and in functional programming languages, like Haskell or Erlang.

| Operation | Stack                             | Comment                    |
|:---------:|:----------------------------------|:---------------------------|
| `;`       | `[ value name:atom <] ---> [  <]` | *name* is bound to *value. |

Examples:

```
42 'theAnswer;               ` The value 42 is bound to theAnswer

[ dup * ] 'square            ` This is how functions are defined.
                             ' Store a list!

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

In Stacky, this becomes:

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

**NOTE:** the lists are not necessary unless a branch needs to execute more than one operation. The example above can be rewritten as:

```
[ swap 65 >= 2 1 ? / ] 'compute_discount;

```

More concise, but the readability may be affected...

### Stack operations

The main purpose of stack operations is to manipulate the stack and/or gain information about the stack.

Stacky implements the following stack operations:

| Operation                       | Comment                                                                             |
|:--------------------------------|:------------------------------------------------------------------------------------|
| [`clear`](#the-clear-operation) | Clear the stack, i.e., removes any information stored on the stack.                 |
| [`depth`](#the-depth-operation) | Return the number of elements on the stack.                                         |
| [`drop`](#the-drop-operation)   | Remove the topmost element on the stack                                             |
| [`lrot`](#the-lrot-operation)   | Rotate the three topmost elements by moving the top element to the third position.. |
| [`nlrot`](#the-nlrot-operation) | Rotate the N topmost elements by moving the top element to the N:th position..      |
| [`nover`](#the-nover-operation) | Copy the N:th topmost element to the top of the stack.                              |
| [`nrot`](#the-nrot-operation)   | Rotate the N topmost elements by moving the N:th element to the top.                |
| [`nswap`](#the-nswap-operation) | Swap (reverse) the N topmost elements                                               |
| [`over`](#the-over-operation)   | Copy the second topmost element to the top of the stack.                            |
| [`rot`](#the-rot-operation)     | Rotate the three topmost elements by moving the third element to the top.           |
| [`swap`](#the-swap-operation)   | Swap the two topmost elements                                                       |




#### The `clear` operation

Clear the stack of all information. 

```
clear : [...<] ---> [<]
```

Example:

```
[ 1 2 3 4 4 5 6 7 <]
> clear
[  <]
```

#### The `depth` operation

Puts the current number of elements (*depth*) onto the top of the stack

```
depth : [x_n ... x_1 <] ---> [x_n ... x_1 n:integer<]
```

Example:

```
[ 10 20 40 80 <]
> depth
[ 10 20 40 80 4 <]
> clear
[  <]
> depth
[ 0 <]
> depth
[ 0 1 <]
> depth
[ 0 1 2 <]
```

#### The `drop` operation

Remove the topmost element from the stack. If the stack is empty, an error will occur.

```
drop : [x <] ---> [ <]
```

Example:

```
[ 1 4 9 <]
> drop
[ 1 4 <]
> drop
[ 1 <]
> drop
[  <]
> drop
ERROR: Stack underflow in operation: 'drop'
```

#### The `ndrop` operation

Remove the *N* topmost element from the stack. If the stack does not contain at least *N* elements, an error will occur.

```
ndrop : [x_1 ... x_n n <] ---> [ <]
```

Example:

```
[ 1 4 9 16 25 <]
> 3 ndrop
[ 1 4 <]
> 0 ndrop
[ 1 4 <]
> 5 ndrop
ERROR: Stack underflow in operation: 'ndrop'
```

**NOTE:** `drop` is equivalent to `1 ndrop`

#### The `over` operation

Copy the second topmost element of the stack to the top of the stack.

```
over : [x y<] ---> [x y x<]
```

Examples:

```
[ 8 16 32 <]
> over
[ 8 16 32 16 <]
> over
[ 8 16 32 16 32 <]
```

#### The `nover` operation

Copy the *N:th* topmost element of the stack to the top of the stack.

```
nover : [x_1 ... x_n n <] ---> [x_1 ... x_n x_1<]
```

Examples:

```
[ 1 2 3 4 5 6 <]
> 3 nover
[ 1 2 3 4 5 6 4 <]
> 7 nover
[ 1 2 3 4 5 6 4 1 <]
> 10 nover
ERROR: Stack underflow in operation: 'nover'
```

**NOTE:** `over` is equivalent to `2 nover`. Furthermore, `dup` is equivalent to `1 nover`.

#### The `rot` operation

Rotate the three topmost elements by moving the third element to the top, shifting the two topmost element one step down.

```
rot : [x y z<] ---> [y z x<]
```

Examples:

```
[ 1 2 3 <]
> rot
[ 2 3 1 <]
> rot 
[ 3 1 2 <]
> rot
[ 1 2 3 <]
```

#### The `lrot` operation

Rotate the three topmost elements by moving the top element to the third position. This is the inverse to the `rot` operation.

```
nrot : [x y z<] ---> [z x y<]
```

Examples:

```
[ 1 2 3 <]
> lrot
[ 3 1 2 <]
> lrot
[ 2 3 1 <]
> lrot
[ 1 2 3 <]
> lrot rot
[ 1 2 3 <]
> rot lrot
[ 1 2 3 <]
```

#### The `nrot` operation

Rotate the *N* topmost elements by moving the *N:th* element to the top, shifting the *N-1* topmost element one step down.

```
nrot : [x_1 x_2 ... x_n n <] ---> [x_2 ... x_n x_1 <]
```

Examples:

```
[ 1 2 3 4 <]
> 4 nrot
[ 2 3 4 1 <]
> 2 nrot
[ 2 3 1 4 <]
```

**NOTE:** `rot` is equivalent to `3 nrot`, and `swap` is equivalent to `2 nrot`.

#### The `nlrot` operation

Rotate the topmost element to the *N:th* position in the stack. This is the inverse of the `nrot` operation.

```
nlrot : [x_1 x_2 ... x_n n <] ---> [x_n x_1 x_2 ... x_(n-1) <]
```

Examples:

```
[ 1 2 3 4 <]
> 4 nlrot
[ 4 1 2 3 <]
> 4 nlrot
[ 3 4 1 2 <]
> 4 nlrot
[ 2 3 4 1 <]
> 4 nlrot
[ 1 2 3 4 <]
> 4 nrot 4 nlrot
[ 1 2 3 4 <]
```

**NOTE:** `lrot` is equivalent to `3 nlrot`, and `swap` is equivalent to `2 nlrot`.

#### The `swap` operation

Swap the two topmost elements by moving the second element to the top, shifting the topmost element one step down.

```
swap : [x y<] ---> [y z<]
```

Examples:

```
[ 1 2 3 <]
> swap
[ 1 3 2 <]
> swap
[ 1 2 3 <]
```

#### The `nswap` operation

Swap the *N* topmost elements by reversing the order of them on the stack.

```
nswap : [x_1 x_2 ... x_(n-1) x_n <] ---> [x_n x_(n-1) ... x_2 x_1 <]
```

Examples:

```
[ 1 2 3 4 5 6 7 8 9 10 <]
> 5 nswap
[ 1 2 3 4 5 10 9 8 7 6 <]
> 3 nswap
[ 1 2 3 4 5 10 9 6 7 8 <]
> 2 nswap
[ 1 2 3 4 5 10 9 6 8 7 <]
```

**NOTE:** `swap` is equivalent to `2 nswap`.

### Sequence operations

Stacky had two sequence data-types: *lists* and *strings*. With some exceptions, most operations on strings, also work on lists and vice versa.

Stacky implements the following common list and string operations:

| Operation                                 | Comment                                              |
|:------------------------------------------|:-----------------------------------------------------|
| [`++`](#concatenation)                    | Concatenate two sequences on the stack to one.       |
| [`length`](#the-length-operation)         | Compute the length of a string or list.              |
| [`fromList`](#the-fromlist-operation)     | Break up a list and put its elements on the stack.   |
| [`fromString`](#the-fromstring-operation) | Break up a string and put its elements on the stack. |
| [`toList`](#the-tolist-operation)         | Build a list from elements on the stack.             |
| [`toString`](#the-tostring-operation)     | Build a list from elements on the stack.             |
| [`toStr`](#the-tostr-operation)           | Convert a list to string.                            |
| [`reverse`](#the-reverse-operation)       | Reverse the elements of a string or list.            |

#### Concatenation

This operation takes two similar sequences on the stack and returns the concatenated sequence

```
++ :: [s2:T(m) s1:T(n)<] ---> [(s1 s2):T(m+n) <]
```
`T` here is either a list or a string. `s1` and `s2` must be the same type, i.e., it is not possible to concatenate a string and a list.

Examples:

```
> "HELLO " "WORLD"
[ "HELLO " "WORLD" <]
> ++
[ "HELLO WORLD" <]

> "" "FOO" ++
[ "FOO" <]

[1 2 3] [4 5 6] ++
[ [1 2 3 4 5 6] <]
```

#### The `length` operation

This operation takes a list or a string and returns the length in characters or elements.

```
length : [ xs:list(n) <] ---> [ n <]
```

#### The `fromList` operation

This operation takes a list on the top of the stack and pushes its elements, as well as the length of the list, to the stack.

```
fromList : [ [x_1 ... x_n] <] ---> [ x1 ... x_n n <]
```

Examples:

```
> [100 200 400 800]
[ [100 200 400 800] <]
> fromList
[ 100 200 400 800 4 <]
> clear
[  <]
> [] fromList
[ 0 <]
```

#### The `fromString` operation


This operation takes a string on the top of the stack and pushes its elements, as well as the length of the string, to the stack.

```
fromString : [ s:string(n) <] ---> [ s1:string(1) ... s_n:string(1) n <]
```

Examples:

```
> "HELLO WORLD" fromString
[ "H" "E" "L" "L" "O" " " "W" "O" "R" "L" "D" 11 <]
```

#### The `toList` operation

This operation pops a number of values off the stack, puts them in a list and pushes that onto the stack.

```
toList :: [x_1 ... x_n n <] ---> [ [x_1 ... x_n] <]
```

**NOTE:** `toList` is the inverse of `fromList` so:

```
fromList toList : [ xs:list <]      ---> [ xs:list <]
toList fomList  : [x_1 ... x_n n <] ---> [ [x_1 ... x_n] <]
```

Examples:

```
> 100 200 300 3 toList
[ [100 200 300] <]
> clear
[  <]
> 'put [1 2 3] "Hello" 3 toList
[ [put [1 2 3] "Hello"] <]
```

#### The `toStr` operation

This operation converts any element to a string in a representation that will convert back to the element if the `eval` operation is performed. This means that the quotation marks of a string is retained in the result.

```
toStr : [ elem <] ---> [ s:string <]
```

Examples:

```
> 43 toStr
[ "43" <]

> "APA" toStr
[ "\"APA\"" <]

> [1 "apa" 222] toStr
[ "[1 \"apa\" 222]" <]

> [ 12 "apa" 444] toStr eval
[ [12 "apa" 444] <]
```

#### The `toString` operation

This operation pops a number of values off the stack, converts them to strings (if they aren't strings already), puts them in a string and pushes that onto the stack.

```
toString :: [x_1 ... x_n n <] ---> [ s:string <]
```

**NOTE:** `toList` is the inverse of `fromList`. so if the elements on the stack are one character strings, then:

```
fromList toList : [ s:string <]      ---> [ s:string <]
toList fomList  : ["c_1" ... "c_n" n <] ---> [ "c_1 ... x_n" <]
```

Examples:

```
> "H" "e" "l" "l" "o" 5 toString
[ "Hello" <]
> clear
[  <]
> 'put [1 2 3] "Hello" 3 toString
[ "put[1 2 3]Hello" <]
> clear
[  <]
> 100 200 300 3 toString
[ "100200300" <]
```

#### The `reverse` operation

This operation takes a list or string and reverses the order of the elements.

```
reverse : [ [x_1 x_2 ... x_(n-1) x_n] <] ---> [ [x_n x_(n-1) ... x_2 x_1] <]
```

Examples:

```
> "HELLO" reverse
[ "OLLEH" <]

> [1 2 3 4] reverse
[ [4 3 2 1] <]

```

### Input/Output operations

The following I/O operations are currently available:

| Operation                             | Comment                                                             |
|:--------------------------------------|:--------------------------------------------------------------------|
| [`input`](#the-input-operation)       | Read a line from *stdin* and push it onto the stack as a string.    |
| [`print`](#the-print-operation)       | Take the topmost value off the stack and print, in verbatim.        |
| [`prompt`](#the-prompt-operation)     | Same as `input` but it prints a user-defined prompt before reading. |
| [`put`](#the-put-operation)           | Put the topmost term (without newline) on stdout.                   |
| [`putLn`](#the-putln-operation)       | Same as `put` but add a newline to the end.                         |
| [`readFile`](#the-readfile-operation) | Read the named file from the file system.                           |

#### The `input` operation

Read a line from *standard input* and store it on the stack as a string. To notify the user it prints the prompt "? ". This is equivalent to `"? " prompt`.

```
input : [ <] ---> [ line:string <]
```

Example:

```
[  <]
> input 
? foo
[ "foo" <]
```

#### The `print` operation

This operation is used to print a term as it is written in stacky on stdout. A newline is added after the printout. This means that strings are printed with double quotes (`"`) and visible escape sequences.

```
print :: [ x <] ---> [ <]
```

Example:

```
[  <]
> 42 print
42
[  <]

[  <]
> "HELLORLD" print
"HELLORLD"
[  <]

[  <]
> [abc def ghi] print
[abc def ghi]
[  <]
> [1 2 3 + ] print
[1 2 3 {+}]
[  <]
```

#### The `prompt` operation

Read a line from *standard input* and store it on the stack as a string. To inform the user as to what is to be read, it takes a string from the top of the stack and uses it as a prompt.

Example:

```
[  <]
> "Width: " prompt "Height: " prompt "Depth: " prompt
Width: 100
Height: 200
Depth: 300
[ "100" "200" "300" <]
```

`prompt` and `input` both allow multiple lines to be read into one string. Just add *backslash* (`\`) to the input before pressing return and you will be prompted for another line. This continues until no more backslashes are added to the input.

```
[  <]
> input 
? 123\
 ... ? 456\
 ... ? 789
[ "123\n456\n789" <]
> clear
[  <]
> "Text: " prompt
Text: abc\
 ... Text: def\
 ... Text: ghi
[ "abc\ndef\nghi" <]
```

#### The `put` operation

This operation is used to print a term on stdout. `put` does however treat strings specially. It will simply output the string as text to the stdout.

No newline character is added to the output so several `put` commands will print to the same line. Special characters in the string will have the actions they usually have, like causing a new line.

```
put :: [ x <] ---> [ <]
```

Example:

```
[  <]
> "HELLO" put " " put "WORLD" put
HELLO WORLD[  <]

[  <]
> "abc\ndef\nghi\n" put
abc
def
ghi
[  <]

[  <]
> [ a b c d] put
[a b c d][  <]

```

#### The `putLn` operation

`putLn` has the same semantics as `put` but it adds a newline to the output.

```
put :: [ x <] ---> [ <]
```

Example:

```
[  <]
> [1 2 foo =] putLn
[1 2 foo {=}]
[  <]
> "HELLORLD" putLn
HELLORLD
[  <]
```

#### The `readFile` operation

### Reflection/introspection operations

Stacky has some support for [reflection](https://en.wikipedia.org/wiki/Reflective_programming) and [type introspection](https://en.wikipedia.org/wiki/Type_introspection).

*Reflection* comes from the fact that, much like the [Lisp programming language](https://en.wikipedia.org/wiki/Lisp_(programming_language)), Stacky can treat data as code, and vice versa. This is due to the fact that [lists](#lists) can be executed using the [apply](#the-apply-operation).

As of now, there are no operations for *type introspection*, but such will be added in the near future.

| Operation                                   | Comment                                                                     |
|:--------------------------------------------|:----------------------------------------------------------------------------|
| [`@`](#the-apply-operation)                 | Evaluate a list or execute a defined name.                                  |
| [`eval`](#the-eval-operation)               | Evaluates a string as if it was code.                                       |
| [`import`](#the-import-operation)           | Reads the contents of a file and executes its contents as code.             |
| [`env`](#the-env-operation)                 | Prints a list of all defined names on stdout.                               |
| [`typeOf`](#the-typeof-operation)           | Returns a string representing the type of the value on the top of the stack |
| [`typeInfo`](#the-typeinfo-operation)       | Returns information about the type of the top element.                      |
| [`expectType`](#The-expecttype-operation)   | Throws an error if the top element does not match a description.            |
| [`expectDepth`](#The-expectdepth-operation) | Throws an error if the depth of the stack is insufficient.                  |
| [`throw`](#The-throw-operation)             | Unconditionally throws an error with supplied message.                      |

#### The apply operation

This operation allows programmer to execute lists and defined names as code. It is automatically applied when stored names are resolved, but manually constructed code snippets must be executed using the *apply* (`@`) operation.

```
@ : [ x:list <] ---> [... <]    `Whatever executing the list evaluates to.
  : [ x:atom <] ---> [... <]    `Whatever executing the atom evaluates to.
```

**NOTE:** If the atom does not contain anything executable, `@` just leaves it on the top of the stack.

Examples:

```
[  <]
> [1 2 3 + +]
[ [1 2 3 {+} {+}] <]
> @
[ 6 <]

> "HELLORLD" 'ahoy;
[ <]
> ahoy
[ "HELLORLD" <]
> 'ahoy
[ "HELLORLD" ahoy <]
> @
[ 6 "HELLORLD" "HELLORLD" <]

[ <]
> 1 2 3 [+ +]
[ 1 2 3 [{+} {+}] <]
> @
[ 6 <]
```

#### The `eval` operation

The `eval` operation takes a string from the top of the stack and executes it as Stacky code.

```
eval : [ str:string <] ---> [... <]    `Whatever executing the string evaluates to.
```

Example:

```
[  <]
> "1 2 3 4 + + +"
[ "1 2 3 4 + + +" <]
> eval
[ 10 <]

[ <]
> "\"HELLO \" \"WORLD\" ++ print" eval
"HELLO WORLD"
[ <]
```

**NOTE:** Code here means **ANY** code that is valid Stacky. This means that `eval` is incredibly powerful. In fact, using `eval` and `prompt`, it is possible to implement the Stacky REPL!

```
> [ "REPL> " prompt eval repl ] 'repl;
[  <]
> repl
REPL> 1 2 3 + + 
REPL> print
6
REPL> [dup *] 'sq;
REPL> 256 sq print
65536
REPL>
```

#### The `import` operation

This operation takes a string off the stack, reads it and executes it as Stacky code. This is useful when creating stand-alone programs and libraries.

```
import : [ fname:string <] ---> [...<]
```

Example:

Loading a module containing a tester for the [Collatz conjecture](https://en.wikipedia.org/wiki/Collatz_conjecture) and then testing it for the number *15*.

```
> "examples/collatz.sy" import
[  <]
> 15 collatz
46
23
70
35
106
53
160
80
40
20
10
5
16
8
4
2
1
N = 17
[  <]
```

#### The `env` operation

This operation prints the bound names in the [environment](#variables-and-the-environment).

```
env : [ <] ---> [ <]
```

Example (after loading the Collatz conjecture tester):
```
> env
"ones" : [1 {swap} onesLoop {drop}]
"onesLoop" : [[{dup} 0 {>}] [{swap} 10 {*} 1 {+} {swap} 1 {-} onesLoop] [] {?}]
"collatz" : [0 {swap} collatzLoop {drop} "N = " {put} {putLn}]
"collatzLoop" : [{swap} 1 {+} {swap} [{dup} isEven] [2 {/}] [3 {*} 1 {+}] {?} {dup} {print} [{dup} 1 {>}] [collatzLoop] [] {?}]
"isEven" : [2 {%} 0 {=}]
"+" : {+}
"-" : {-}
"*" : {*}
...
```

**NOTE:** Items enclosed in brackets are built-in operations, that are treated somewhat differently by the Stacky interpreter.

#### The `typeOf` operation

This operation takes the top element of the stack and returns a string representing the type of the element. Currently one of: `"atom"`, `"integer"`, `"list"`, or `"string"`.

```
typeOf : [ val <] ---> [ type:string <]
```

Examples:

```
> 42 typeOf
[ "integer" <]
> clear
[  <]
> "Hello" typeOf
[ "string" <]
```

#### The `typeInfo` operation

This operation takes the top element of the stack and returns a string representing the type of the element and a size. The type string is the same as returned by [`typeOf](#the-typeof-operation). 

The integer is one (1) for all types except lists and strings, for which it is the number of elements in the object.

```
typeInfo : [ val <] ---> [ type:string size:integer<]
```

Examples:

```
> 42 typeInfo
[ "integer" 1 <]
> clear
[  <]
> "The answer" typeInfo
[ "string" 10 <]

```

#### The `expectType` operation

This operation tests the top element against a type specification. If the type of the element matches, it keeps the element on the stack, but if there is a mismatch an error occurs.

```
             [ val [type:string minSize:integer maxSize:integer] <] ---> [ val <]
expectType : or
             [ val [type:string minSize:integer maxSize:integer name:string] <] ---> [ val <]
```

**NOTE:** The size parameters must be present even if the type is size-less, like an integer. In such case any values *m* and *n*, such that *m <= 1 < n* will work. Furthermore an infinite upper range is denoted by *-1*, so `["list 5 -1]` means any list with five or more elements.

If the *name* parameter is provided, the error message will contain the name which is useful when debugging error messages.

Examples:

```
> "Hello" ["string" 0 -1] expectType
[ "Hello" <]
> ["string" 4 7] expectType
[ "Hello" <]
> ["string" 8 -1] expectType
ERROR: Operation 'expectType' expects a value of type 'string(8,-1)', got '"Hello" : string(5)'
> clear
[  <]
> 42 ["integer" 1 2] expectType
[ 42 <]
> ["atom" 1 2] expectType
ERROR: Operation 'expectType' expects a value of type 'atom(1,2)', got '42 : integer(1)'
> 42 ["atom" 1 2 "foo"] expectType
ERROR: Operation 'foo' expects a value of type 'atom(1,2)', got '42 : integer(1)'
```

#### The `expectDepth` operation

This operation tests the top element against a type specification. If the type of the element matches, it keeps the element on the stack, but if there is a mismatch an error occurs.

```
             [ val [type:string minSize:integer maxSize:integer] <] ---> [ val <]
expectType : or
             [ val [type:string minSize:integer maxSize:integer name:string] <] ---> [ val <]
```

**NOTE:** The size parameters must be present even if the type is size-less, like an integer. In such case any values *m* and *n*, such that *m <= 1 < n* will work. Furthermore an infinite upper range is denoted by *-1*, so `["list 5 -1]` means any list with five or more elements.

If the *name* parameter is provided, the error message will contain the name which is useful when debugging error messages.

Examples:

```
> "Hello" ["string" 0 -1] expectType
[ "Hello" <]
> ["string" 4 7] expectType
[ "Hello" <]
> ["string" 8 -1] expectType
ERROR: Operation 'expectType' expects a value of type 'string(8,-1)', got '"Hello" : string(5)'
> clear
[  <]
> 42 ["integer" 1 2] expectType
[ 42 <]
> ["atom" 1 2] expectType
ERROR: Operation 'expectType' expects a value of type 'atom(1,2)', got '42 : integer(1)'
> 42 ["atom" 1 2 "foo"] expectType
ERROR: Operation 'foo' expects a value of type 'atom(1,2)', got '42 : integer(1)'

```

## The `throw` operation

This operating throws an error, given a message and an operation's name.

```
throw : [ [message:string name:string] <] ---> ERROR
```

Example:

```
> ["This is an error" "foo"] throw
ERROR: In 'foo': This is an error
> 
```

## Syntax description

```
Commands         <-- Command*

Command          <-- Integer
                 <-- Atom
                 <-- String
                 <-- List

List             <-- '[' Commands ']'

Integer          <-- [-]?[0-9]+

Atom             <-- [a-zA-Z][a-zA-Z0-9_]*
                 <-- BuiltIn-Operator

String           <-- '"' Character* '"'

BuiltIn-Operator <-- '+' | '-' | '*' | '/' | '=' | '<>' | '<' | '>' | '<=' | '>=' | '~' 

Character        <-- ASCII-Character
                 <-- Escape-Sequence

Escape-Sequence  <-- '\'[\"rnt]
```
