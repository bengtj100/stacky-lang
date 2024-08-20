# Stacky - A simple stack language
## Language Introduction
### Version 0.3.x

## Disclaimer

The Stacky language is still in beta version 0.3.x, so things are moving fast and all versions below 1.0 may break **backwards compatability without notice**!

**USE THIS LANGUAGE AT YOUR OWN RISK!**

**IT SHOULD NOT BE CONSIDERED FOR PRODUCTION DEPLOYMENTS UNTIL FURTHER NOTICE!!!**

## Introduction

This is an introduction to the *Stacky* programming language and its implementation. It is assumed that the reader has read the installation instructions and has installed the interpreter on their computer. It is also assumed that the user has a functioning text editor and basic computer skills.

### History

### Stacks and stack-based languages

The [stack](https://en.wikipedia.org/wiki/Stack_(abstract_data_type)) is a very common data structure. It is used in computer hardware as well as in most programming languages to store function parameters, return addresses, etc. However, the use of the stack in, say C++, is mostly hidden.

Not so in *Stacky* and other [stack oriented](https://en.wikipedia.org/wiki/Stack-oriented_programming) languages, like [Forth](https://en.wikipedia.org/wiki/Forth_(programming_language)). Here, the stack, or multiple stacks, plays an important and not least of all, a very visible role.

First, let us get aquainted with the [stack](https://en.wikipedia.org/wiki/Stack_(abstract_data_type))!

As mentioned above, a stack is an abstract data type. It is a container type that can hold several elements which are added to and deleted from the stack in a last-in first-out manner.

Stacks have two princibal operations; *push* that adds an element to the stack, and *pop* that removes the lastest added element from the stack.

Below, you find a diagram showing the integers 5, 11, and 3 being pushed to a stack.

```
                                                           +----+
                                                           |  3 |
                                        +----+             +----+
                                        | 11 |             | 11 |
                     +----+             +----+             +----+
            push     |  5 |    push     |  5 |    push     |  5 |
--+----+-- ------> --+----+-- ------> --+----+-- ------> --+----+--
             ^                  ^                  ^
             |                  |                  |
             |                  |                  |
             5                 11                  3
```

Next, lets pop the elements from the stack, then we get the elements 3, 11, and 5 before the stack becomes empty. Note that we get them in the *opposite* order we pushed them onto the stack

```
  +----+
  |  3 |
  +----+             +----+
  | 11 |             | 11 |
  +----+             +----+             +----+
  |  5 |    pop      |  5 |    pop      |  5 |    pop
--+----+-- ------> --+----+-- ------> --+----+-- ------> --+----+--
             |                  |                  |
             |                  |                  |
             V                  V                  V
             3                 11                  5
```

In stack oriented languages, like Stacky and Forth, almost all operations operate directly on the stack(s). They take input arguments from the stack and push results back onto the stack. So to cmpute *1+2*, we have to push the *1* and *2* to the stack before executing the addition operation. In Stacky, this becomes `1 2 +`. 

```
                                        +----+
                                        | 11 |
                     +----+             +----+             +----+
              1      |  1 |      2      |  2 |      +      |  3 |
--+----+-- ------> --+----+-- ------> --+----+-- ------> --+----+--
```

## Using the interpreter

When the Stacky intepreter is installed and put in a directory on the path, it is started by simply typing `stacky` into a command-line shell. Doing that will give you an output similar to this:

```
$ stacky

Stacky, version: 0.1, build: 2024-07-15.15-54-12
Copyright (c) 2024 Bengt Johansson -- All rights reserved


Loading the Prelude ... DONE

[  <]
> 
```

This consists of four parts:

1. The greeting and copyright to show that the correct version of stacky has started.
2. A message showing that the prelude, (a library of often used operations), has been successfully loaded into the system.
3. A representation of the stack. Everyting between `[` and `<]` are the elements on the stack, on which the operations operate. When Stacky is started, the stack is normally empty.
4. A command prompt `> ` where you can enter Stacky commands and have them evaluated.

Pressing `CTRL-D` or `CTRL-C` terminates the interpreter and takes you back to the shell.

```
[  <]
> ^D 
Leaving stacky interpreter
Bye!

$ 
```

### Evaluating expressions

Once we have started Stacky, it is ready to accept commands. Let's us evaluate a simple expression; *1 + 2*!

As mentioned before, the addition (`+`) takes its arguments from the stack, so we need to push the *1* and *2* onto the stack beforehand. This is done by simply typing an integer and pressing return. Let's do that to both of them:

```
[  <]
> 1
[ 1 <]
> 2
[ 1 2 <]
```

We now have both integers on the stack. The *1* is on the bottom and the *2* is on the top of the stack. So given how we represent the stack, we can conclude that `[` shows the bottom and `<]` denotes the top.

Now, let's execute the addition. This is done by typing a plus `+` and pressing return. This will cause Stacky to pop two integers from the stack, perform the addition, and then pushing the result back onto the stack.

```
[ 1 2 <]
> +
[ 3 <]
```

To make life easier for the user, Stacky allows us to enter several operations on the same line and execute them at once when return is pressed. Thus we can write `1 2 +` to perform the addition:

```
[  <]
> 1 2 +
[ 3 <]
```

To make it easier to enter long expressions, ending a line with a backslash (`\`), allows the user to continue input before execution takes place. There is no fixed upper limit to how long input is allowed, but it is limited by the available memory.

Example:

```
[  <]
> "This is a long string. "\
 ... > "Another long string. "\
 ... > "MORE CHARACTERS"\
 ... > ++\
 ... > ++
[ "This is a long string. Another long string. MORE CHARACTERS" <]
```

### My first program

Next, let's write a program in Stacky, and have the interpreter execute it. The first program someone writes in a new language is often the so called *Hello World* program, wich - as its name implies - just prints "Hello World" on the terminal and terminates.

Open your favourite text editor and prepare a file. Let's call it `hello.sy`. In this file write the following two lines:

~~~
```
"Hello World!" putLn
~~~

Now lets save the file and from the shell issue the command `stacky -b hello.sy` to see the result:

```
$ stacky -b hello.sy
HELLO WORLD
$ 
```

The option `-b` or `--batch` makes Stacky work in batch mode. In this mode the program in the file(s) given is executed and then the interpreter terminates.

Stacky programs can also be loaded using a *shebang* `#!`. Here is an example of the Hello World program as a stand-alone script:

~~~
#!/usr/bin/env -S stacky -b
```
"HELLO WORLD" putLn
~~~

Save it to a file called `hello.sy`, make it executable. Then it will run:

```
$ chmod +x hello.sy 
$ ./hello.sy 
HELLO WORLD
$ 
```

Issuing `stacky --help` will list the available options.

## Quick tutorial

After this tutorial, you should have a working understanding of most concepts in Stacky. It should be possible to read the reference manual and the prelude for more information.

### Let's get stackin'

Since Stacky is a stack oriented language, let's start by looking at the stack. We know how to compute simple expressions like *1 + 2*. Now we will tackle a a slightly trickier one: *2 + 3 \* 4*.

Using Stacky, we won't have to worry about [PEMDAS](https://en.wikipedia.org/wiki/Order_of_operations) too much. But as we all know multiplication has higher priority than addition, so that is evaluated first. We evaluate this expression as follows:

```
1 + 2 * 3 ---> 1 + 6 ---> 7
```

Using the stack we can decide when an operation is to be performed. Lets start pushing the numbers:

```
> 1 2
[ 1 2 <]
```

Since we need to evaluate the multiplication first, let's push the *3* as well. 

```
> 3
[ 1 2 3 <]
```

Now we can apply the multiplication and additon:

```
[ 1 2 3 <]
> *
[ 1 6 <]
> +
[ 7 <]
```

Of course, we could have entered the expression directly:

```
[  <]
> 1 2 3 * +
[ 7 <]
```

So far, all values have been conveniently placed on the stack for our operations to consume. Let's take a little more complex problem: The [Pytagorean theorem](https://en.wikipedia.org/wiki/Pythagorean_theorem) states that *"... the area of the square whose side is the hypotenuse (the side opposite the right angle) is equal to the sum of the areas of the squares on the other two sides." [*[Wikipedia]*](https://en.wikipedia.org/wiki/Pythagorean_theorem)

If the sides are *a*, *b* and the hypotenuse is *c*, we get the following equation.

$a^2 + b^2 = c^2$

So if the sides are 3 and 4, the hypotenuse is 5. Let's use Stacky to compute the equation.

Let's put the values onto the stack:

```
> 3 4 5
[ 3 4 5 <]
```

Now we need to figure out a way to compute the square of a number. Stacky has an operation called `dup` that duplicates whats on top of the stack. Once we have two of the same we can use multiplication to get the square:

```
[ 3 4 5 <]
> dup
[ 3 4 5 5 <]
> *
[ 3 4 25 <]
```

There we have one square. Now we have to square the sides. The `rot` operation rotates the top three elements on the stack:

```
[ 3 4 25 <]
> rot
[ 4 25 3 <]
```

Now that we have one of sides available, we can repeat the squaring operation:

```
[ 4 25 3 <]
> dup *
[ 4 25 9 <]
```

Let's do the same for the last side:

```
[ 4 25 9 <]
> rot dup *
[ 25 9 16 <]
```

Now we can add the sides together:

```
[ 25 9 16 <]
> + 
[ 25 25 <]
```

While it's obvious for us that the equation holds, we use the equality operation to compare them, so that Stacky knows that too:

```
[ 25 25 <]
> =
[ 1 <]
```

Stacky doesn't have a boolean type for truth values. It uses a few values in different types to denote *false*. These are : `0` (zero integer), `[]` (the empty list), and `""` (the empty string). All other values denote *true*

As we got a one here, we know that the comparison held. Boolean operations only return `0` for false and `1` for true.

### Programmers are lazy (and so is Stacky sometimes)

In the above example, we repeated a number of operations more than once. In fact the entire computatio became:

```
[  <]
> 3 4 5 dup * rot dup * rot dup * + =
[ 1 <]
```

That's a lot of repeated work. Stacky allows us to define new operations by storing code snippets in lists. Stacky will automatically execute the code when the variable is retieved.

We can start with the squaring operation:

```
[dup *] 'square;
```

The `[dup *]` is the body of the operation. `'square` pushes the name "square" onto the stack. (The `'` tells Stacky not to try to look up and rewrite the name, should it exist in an outer scope. When using atoms as values they shall be prefixed by an inhibitor! Finally `;` stores the operation. 

We kan now square integers:

```
[  <]
> 5 square
[ 25 <]
> 40 square
[ 25 1600 <]
> square
[ 25 2560000 <]
```

The test now becomes:

```
> 3 4 5 square rot square rot square + =
[ 1 <]
```

We can store the entire test in a function, so that we can test other sets of values:

```
> [square rot square rot square + =] 'pyt;
[  <]

> 3 4 5 pyt
[ 1 <]

> 3 4 6 pyt
[ 0 <]
```

Ones and zeros might not be the nicest result for humans. We would like to get "GOOD" or "BAD" as result. Stacky has the conditional operation `?` for that purpose:

```
[  <]
> [3 4 5 pyt] "GOOD" "BAD" ?
[ "GOOD" <]

[  <]
> [3 4 6 pyt] "GOOD" "BAD" ?
[ "BAD" <]
```

The conditional operation takes three values from the stack. First it evaluates the third element down. If it is true, it evaluates the second, otherwise the first element is evaluated. This is the if - then - else construct found in other languages.

Lets make a function out of this:

```
> [ [pyt] "GOOD" "BAD" ? ] 'pyt2;

[  <]
> 3 4 5 pyt2
[ "GOOD" <]

> 4 7 9 pyt2
[ "BAD" <]
```

To make the test even easier to use, we kan output the result on stdout. That way we can write a program that tests a number of combinations. We will use the `put` and `putLn` operations.

```
> [3 ndup 3 toList put pyt2 " is " put putLn]'pyt3;
[  <]
> 3 4 5 pyt3
[3 4 5] is GOOD
[  <]
> 3 4 6 pyt3
[3 4 6] is BAD
[  <]
```

`ndup` works like `dup`, but can work on more than one element. `3 ndup` duplicates the three topmost element on the stack.

The file `examples/pytagoras.sy` contains a program with all these definitions. Try running it:
```
$ stacky -b examples/pytagoras.sy -e test
[3 4 5] is GOOD
[3 4 6] is BAD
[111 148 185] is GOOD
$ 
```

The `-e` (or `--eval`) is used to evaluate a Stacky expression. In this case to execute an operation called `test`.

### Let's get interactive

We know that it is possible to print information from Stacky programs, we will now show how to input data to programs.

The most versatile operation to enter data into Stacky is the `prompt` operation. It takes a string which it uses as a prompt. It then reads data from stdin. It reads until a newline occurs.

It is possible to enter long inputs by putting a backslash at the end of the line, much like in the interpeter.

```
> "Data:" prompt
Data:foo
[ "foo" <]
> "Hello: " prompt
Hello: abc\
 ... Hello: def\
 ... Hello: ghi\
 ... Hello: jkl
[ "foo" "abc\ndef\nghi\njkl" <]
```

In the `examples/pytagoras.sy` file there is an operation called `pyt4` that tests the equation interactivly:

```
[
    "" putLn
    "=====================================" putLn
    "Test the Pytagorean equation"          putLn
    "=====================================" putLn
    "" putLn
    "Enter first side:     " prompt eval
    "Enter second side:    " prompt eval
    "Enter the hypotenuse: " prompt eval
    "" putLn
    pyt3
    
    "" putLn
    "Do one more? (Y/n)" prompt
    ["n" <>]
        [pyt4]
        ["Bye-bye" putLn]
        ?
] 'pyt4;
```

`eval` is used to evaluate a string as a Stacky expression. Here it is used to convert a string to an integer.

Note the lack of loop operations. Currently there are no such in the language. Therefore, we use recursion in `pyt4`.

Let's try it out!

```
 $ stacky -b examples/pytagoras.sy -e pyt4

=====================================
Test the Pytagorean equation
=====================================

Enter first side:     3
Enter second side:    4
Enter the hypotenuse: 5

[3 4 5] is GOOD

Do one more? (Y/n)

=====================================
Test the Pytagorean equation
=====================================

Enter first side:     5
Enter second side:    67
Enter the hypotenuse: 6

[5 67 6] is BAD

Do one more? (Y/n)n
Bye-bye
$ 
```

### Lists are useful

A list is simply a sequence of Stacky values. They are printed enclosed in square brackets (`[` and `]`). The simplest way to construct a list is to enter it with the brackets. This creates a simple list:

```
> [1 2 3 4]
[ [1 2 3 4] <]
```

The `length` operation is used to find out how many elements are in a list:

```
> [1 4 9 16 25] length
[ 5 <]
```

Another way to create a list is using the `toList` operation. It takes an integer *n* and then puts the *n* next elements on the stack into a list:

```
> 1 9 2 8 3 7 4 6 5
[ 1 9 2 8 3 7 4 6 5 <]
> 9 toList
[ [1 9 2 8 3 7 4 6 5] <]
```

There are a number ow ways to take a list apart to get at the individual elements. The easiest is probably the `fromList` operation that takes a list and pushes the elements onto the stack. It also puts the length of the list on the stack.

```
> [ 100 220 333 400 ] fromList
[ 100 220 333 400 4 <]
```

Examples of other operations that work on lists are listed in the Reference manual, but also in the Prelude.

```
> [1 4 9 16 25 36 49 64 81 100] 'squares;
[  <]

> ^squares head
[ 1 <]

> ^squares tail
[ [4 9 16 25 36 49 64 81 100] <]

> ^squares init
[ [1 4 9 16 25 36 49 64 81] <]

> ^squares last
[ 100 <]
```

In the prelude you will also find operations that mimic some of the functions like `map` and `filter` that operate on lists. Take map for instance. It takes a list and then applies an operation on each of the lists. If we want to take the squares of all elements in a list, we can use `map`.

```
> [1 2 3 4 5 6 7 8 9 10] [dup *] map
[ [1 4 9 16 25 36 49 64 81 100] <]
```

The first component `[1 2 3 4 5 6 7 8 9 10]` is our input list. The list `[dup *]` will duplicate the element on top of the stack and then multipy it with it self, thus giving us the square. This is done for all elements by the `map` operation.

### Lists as code

The last example in the previous section showed us another use of lists; to hold snippets of code! If you think of it, we have seen it before. We have defined our own operations, like the `square` operation:

```
[dup *] 'square;
```

We have also seen lists as code in the conditional operation:

```
[x 0 >=] ["X is positive" putLn] ["X is negative" putLn] ?
```

Though, we haven't really gone into how this works. In fact, it all has to do with the apply operation (`@`). It works like this: If it finds a list it will treat each of the elements as if it is code. This even works with lists of integers. Each element will be pushed onto the stack in the order they appear in the list:

```
> [1 2 3 4 5] @
[ 1 2 3 4 5 <]
```

But how come we can create new operations with list without having the contents to execute while we define them? 

```
> dup *
ERROR: Stack underflow in operation: 'dup'
```

This is because when you create a list using the square brackets, the contents go into the list directly without being executed:

```
> [dup *]
[ [{dup} {*}] <]
```

The curly brackets just denote that `dup` and `*` are built-in operations. This will go away in future versions. 

We now have a list fo code on the stack. So if we put an integer below it on the stack using `swap` and then uses `@`, it will be evaluated:

```
> [dup *]
[ [{dup} {*}] <]
> 256 swap
[ 256 [{dup} {*}] <]
> @
[ 65536 <]
```

However, most of the time, we just store the code snippet in a variable for later use:

```
> [dup *]
[ [{dup} {*}] <]
> 'square;
[  <]
> 5 square
[ 25 <]
> 100 square
[ 25 10000 <]
```

It is worth noting that when a list is retrieved from a variable, it is automatically executed! This means that lists that store data will have its data splashed onto the stack immediatelly:

```
> [10 20 30 40 50 60] 'myList;
[  <]
> myList
[ 10 20 30 40 50 60 <]
```

That's why we have the caret (`^`) operation that will retrieve the list to the stack:

```
> ^myList
[ [10 20 30 40 50 60] <]
```

This makes Stacky [homoiconic](https://en.wikipedia.org/wiki/Homoiconicity), i.e. data and code use the same representation. This is a property that it shares with languages in the LISP family.

Suppose we want to create an operation named `double` that doubles an integer value. We can of course create it directly as `[dup +]'double;`. However let's take the `square` operation and use it as a template. We will retrieve the code. Put it onto the stack and replace `*` with `+` and then store it.

```
> [dup *]'square;
[  <]
> 5 square
[ 25 <]
> ^square
[ 25 [{dup} {*}] <]
> fromList
[ 25 {dup} {*} 2 <]
> drop drop
[ 25 {dup} <]
> '+
[ 25 {dup} + <]
> 2 toList
[ 25 [{dup} +] <]
> 'double;
[ 25 <]
> 5 double
[ 25 10 <]
> 5 square
[ 25 10 25 <]
```

In the `examples` directory, you will find more examples of lists as code.


# MORE TO COME
