# Revision history for stacky

## 0.2.3 -- Preliminary

* Atoms starting with and containing underscores are now allowed.
  - **Correction of bug in v0.2.2** that didn't allow atoms with underscores in them at all.
  - Extension of atoms to allow the first character to be an underscore.
* Added STACKY_LIBRARY_PATH and options to control where the Prelude file(s) are loaded from.
* Added support for multiple prelude files to install and release scripts and Makefile.
* Added `__POS__` meta operation that can be used to report current position in a file.
* Added 'Division by zero error' to the interpreter and constants for floating point infinity.
* Added the `global` operation that stashes values in the global scope.
* Added the `catch` operation to handle run-time errors.
* Positions now count from line and char one (1) and not zero (0) as before.
* Unit test framework MiniTest.sy released.
* Added variables that can be updated using the `;=;` and `UPDATE` operations.

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
