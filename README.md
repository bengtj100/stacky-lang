# Stacky - A simple stack language
**Welcome to the home of the *Stacky* programming language!**

## Introduction

**NOTE:** This is very much in alpha state! Things will change and may break compatibility before v1.0! **Do not use in production environments!**

*Stacky* is a small, but quite expressive stack-based programming language that also incorporates some of the features of the Lisp family of languages.

It is intended to be small and simple to implement and write code in, and should be suitable as a high-level programming language for small computer systems, such as single-board computers and micro-controllers.

The reference implementation is an interpreter, but there should be nothing that prevents a compiler to be implemented in the future.

## Example

Here is an implementation of the *Fizz Buzz* algorithm in Stacky

```
[
    'n;
    
    1 n 1
    [
        [
            [dup 15 % 0 =] ["Fizz Buzz"]
            [dup  5 % 0 =] ["Buzz"]
            [dup  3 % 0 =] ["Fizz"]
            [true]         [dup]
        ]
        cond
        putLn
        drop
    ]
    for
]'fizzbuzz;

100 fizzbuzz
```

Stacky is designed for [literate programming](https://en.wikipedia.org/wiki/Literate_programming) and integrates very well with markup languages like HTML, Markdown, and LaTeX. A full literate example can be found [here](https://github.com/bengtj100/stacky-lang/blob/main/examples/fizzbuzz.sy).

## Documentation

Installation and building instructions are available in the [installation guide](https://github.com/bengtj100/stacky-lang/blob/main/doc/Installation.md).

An introduction to using the Stacky interpreter, as well as a short tutorial is available in the [introductory guide](https://github.com/bengtj100/stacky-lang/blob/main/doc/Introduction.md).

There is also a [reference manual](https://github.com/bengtj100/stacky-lang/blob/main/doc/Reference.md) as well as a documented [prelude](https://github.com/bengtj100/stacky-lang/blob/main/prelude/Prelude.sy) (a library of standard operations written in Stacky itself).

There is also a list of known build and run-time [dependencies](https://github.com/bengtj100/stacky-lang/blob/main/DEPENDENCIES.md) to this project.

## Downloads

Binary downloads and releases are available from the [Stacky GitHub release page](https://github.com/bengtj100/stacky-lang/releases).

## Copyright

The *Stacky* project and all its parts are Copyright (c) 2024 Bengt Johansson <bengtj100 at gmail dot com> - All Rights Reserved.

For licensing details, see the [license file](https://github.com/bengtj100/stacky-lang/blob/main/LICENSE).
