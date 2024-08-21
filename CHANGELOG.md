# Revision history for Stacky

## 0.3.1 -- PRELIMINARY

### Language changes

* Added loop operations `for` and `foreach` to the Prelude.
* Added operations `inline` and `clearLocal` to be able to implement manual tail-calls

## 0.3 -- 2024-08-20

### Language changes

* **BREAKING CHANGE:** There will be an error if you do apply on an unbound name! It is still possible to use atoms as values, just inhibit them with the single quite inhibitor.
  * The prelude operation `isDef` can be used to determine if a name is bound or not
* Added the 'find' operation that find substrings in strings.
* Added the operation 'argv' that returns a list of command-line arguments given to the interpreter.
* Added the `getEnv`, `getEnvSafe`, and `setEnv` operations to handle system environment variables.

### Implementation changes

* Added the '--' option to allow command line arguments to be piped into the running program.


## 0.2.3 -- 2024-08-19

### BUG FIXES

* Atoms starting with and containing underscores are now allowed.
  - **Correction of bug in v0.2.2** that didn't allow atoms with underscores in 

### Language changes

* Atoms starting with and containing underscores are now allowed.
  - Extension of atoms to allow the first character to be an underscore.
* Added variables that can be updated using the `;=;` and `UPDATE` operations.
* Added `__POS__` meta operation that can be used to report current position in a file.
* Added the `__CALLPOS__` operation that gives the location the current operation was called from.
* Added the `global` operation that stashes values in the global scope.
* Added the `catch` operation to handle run-time errors.
* `throw` now takes its arguments as individual elements on the stack and is compatible with catch thus `[... throw] [throw] catch = ... throw`.
* Added the `quit`, `exit`, and `error` operations to terminate execution.
* Added constants for floating point infinity.
* Positions now count from line and char one (1) and not zero (0) as before.
* Operations `input` and `prompt` now throws catchable errors

### Implementation changes

* Added `STACKY_LIBRARY_PATH` and options to control where the Prelude file(s) are loaded from.
* Added support for multiple prelude files to install and release scripts and Makefile.
* Added 'Division by zero error' to the interpreter, so that such errors can be caught.
* Replaced REPL written in Haskell with one written in Stacky.

### Other changes

* Unit test framework MiniTest.sy released.
* Automated language tests added. Will run on `make test` and important operations like `release`
  - Modified MiniTest to take advantage of `__CALLPOS__`. Thus no more `__POS__` in assert statements.

* Modified the build system to be able to build withhout installing hasktags.

## 0.2.2 -- 2024-08-11

* **MAJOR BUG FIX!** The append operation will now be parsed as "++" and not two "+" as in version 0.2.1
* Added factorial operation (`!`) to the language
* Minor typos in error messages corrected

## 0.2.1 -- 2024-08-10

* Minor bug-fixes in code and documentation.
* Complete rewrite of the compiler part of the interpreter using a simple parsing combinator library.
* Improved source code documentation in the actual source files.

## 0.2 -- 2024-07-31

* `^` now works on builtins, so `^+` puts the call to the builtin on the stack.
* Added support for floating point numbers and operations on them.

## 0.1 -- 2024-07-25

* First version. Released on an unsuspecting world.
