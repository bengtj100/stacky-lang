# Stacky - A simple stack language
## Language Introduction
### Version 0.1

## Disclaimer

The Stacky language is still in beta version 0.1, so things are moving fast and all versions below 1.0 may break **backwards compatability without notice**!

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

Issuing `stacky --help` will list the available options.

## Quick tutorial

After this tutorial, you should have a working understanding of most concepts in Stacky. It should be possible to read the reference manual and the prelude for more information.

### Datatypes

### Variables

### Functions

### I/O

### Control flow and recursion
