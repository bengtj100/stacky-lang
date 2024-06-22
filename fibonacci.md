# Example: Fibonacci

```
` [
`  'n;
`  [ [dup 0 >] ['i; over over + i 1 - fib_] [] ? ] 'fib_;
`  1 1 n fib_
` ] 'fib;


[
  'n;
  clear
  n fib
] 'test;

[
  [
    [dup 2 <]
        [drop 1 1]
        [1 - fib_ over over + rot drop]
        ?
  ] 'fib_;

  fib_ swap drop

] 'fib;

[ [ [over over >=] [dup fib print 1 + nfib_][]?]'nfib_; 0 nfib_ drop drop]'nfib;
