# Mickey

A statically-typed, imperative programming language that compiles to MIC-1 assembly. Also includes an assembler, linker, and simulator for the MIC-1 architecture.

# How to use

Write Mickey code in .mky files. Then run the `mickey` script, which will compile, assemble, and link the given files. The output will be in a file called `(name of 1st file given to mickey).exe`, which can be run with `dune exec simulator (file).exe`.

`dune exec mickey [file] [output]` takes in a Mickey source file and produces MIC-1 assembly as output.

`dune exec assembler [file] [output]` takes in an assembly file and produces an object file, with no names resolved yet.

`dune exec linker [files] [output]` takes in object files and produces an executable. If there are names that can't be resolved, an error will be thrown.

`dune exec simulator [file]` takes in an executable and runs it. When the HALT instruction is encountered, it enters into a debugger. `ac`, `sp`, and `pc` push those registers onto the stack (debugger stack, not the MIC-1 stack), `@` and `!` read from and write to the memory address on top of the stack like in Forth, `.` prints the top of stack, `c` continues running the program, and `q` quits.

# Language showcase

```
// comment

import "list.mks" // import to get declarations from .mks files (equivalent of .h files in C)

type t = { // struct types
	left: int,
	right: int
}
type t2 = int // only struct type definitions are generative, anything else is an alias
type t3 // there are also opaque types

var hello: char ptr = "Hello, world!" // global variables
var array: int ptr = [2, 34, 6, 43, 43, 5] // array literals can only consist of "simple values" like ints, chars, bools, etc.

fun factorial(x: int): int {
	if(x <= 0)
		1 // notice lack of returns: last expression in a function is what is returned (and if and while loops are expressions too)
	else
		x * factorial(x - 1)
}

fun main(): int { // this is the signature the main function must have
	var x: int = factorial(5); // local variable definitions must come before all other code in a function; be warned that strings and array literals cannot be assigned to local variables
	var l: list = list_new(1, list_new(2, list_new(3, list_empty))); // list is a struct, but it's exported from list.mks as an opaque type
	println(hello); // i/o functions provided in stdlib.mky, automatically imported
	print("The factorial of 5 is: ");
	print(int_to_str(x));
	printchar('\n');
	if(true & !false | true) { // booleans and their operators, a separate type from ints
		break(5 * 67 / 7); // break halts the program with the argument in the accumulator; in the debugger, type "ac ."
		while(x < 200)
			x = x + 1; // like in C, single expression loop bodies don't need braces
		println(int_to_str(list_length(l)));
		println(int_to_str(l as int)); // type casting: try to use it sparingly!
		println(int_to_str({x = x + 1; x})); // block expressions
		array[4] = 8;
		println(int_to_str(array[4])) // no semicolon after the last expression in a block
	}; // semicolons come after blocks too
	list_free(l); // structs are malloc'd, make sure to free them!
	heap_report(); // let's look at the heap
	0
}
```