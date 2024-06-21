# Examples

## Copyright

Copyright (c) 2024 Bengt Johansson <bengtj100 at gmail dot com>.
All rights reserved.

This file is part of the stacky project and its use is regulated by
the conditions stipulated in the file named 'LICENCE', located in the
top directory of said project.

## Introduction

This files contains som examples that can be used in the interpreter

## Examples

### Square function

This function duplicates the top element of the stack and then
multiples with themselves, thus returning the square of the input.

```
[dup *] 'sq;
```

### Fibonacci sequence

This function computes the n+2 first numbers in the Fibonacci sequence
and places them on the stack.

```
[ [ [dup 0 >] ['x; over over + x 1 - f] [] ? ] 'f;  1 swap 1 swap f drop ] fib;
```

