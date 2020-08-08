mol
=====

This is a compiler for mol, a toy language that might actually compile one day.

The frontend is in Rust, and the goal is to use [Inkwell's](https://github.com/TheDan64/inkwell) LLVM wrappers. 
I used the [Rust implementation](https://github.com/TheDan64/inkwell/tree/master/examples/kaleidoscope)
of the [LLVM Kaleidoscope tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html) that is in Inkwell's repository as a 
model to get started from.