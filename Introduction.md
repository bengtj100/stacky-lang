<center>
# Stacky - A simple stack language
## Language Introduction
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
