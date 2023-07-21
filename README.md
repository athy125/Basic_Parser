```markdown
# C-Like Language Parser in Haskell

This repository contains a C-like language parser implemented in Haskell using the Parsec library. The parser can handle a subset of the C language, including variable declarations, assignments, arithmetic expressions, if statements, while loops, function declarations, and function calls.

## Features

- Parses C-like code to create an Abstract Syntax Tree (AST) representing the program's structure.
- Supports basic arithmetic expressions with addition, subtraction, multiplication, and division.
- Handles if statements with optional else branches.
- Handles while loops with a loop body.
- Supports function declarations with parameters and function bodies.
- Handles function calls with arguments.

## Requirements

- Haskell GHC (Glasgow Haskell Compiler)
- Parsec library (`parsec` package)

## Usage

1. Clone the repository to your local machine:

```
git clone https://github.com/athy125/Basic_Parser.git
cd Basic_Parser
```

2. Run the `Main.hs` file to parse the example C-like code:

```
ghc Main.hs
./Main
```

3. Modify the `Main.hs` file to parse your own C-like code.

## Example

Here's an example of C-like code that can be parsed:

```c
var x = 10;
var y = 20;
var z = x + y;
if (z > 15) {
  var a = 100;
  var b = a + 50;
} else {
  var c = 5;
}
while (z > 0) {
  z = z - 1;
}
func add(a, b) {
  return a + b;
}
var result = add(x, y);
```

## Parsing Results

The parsed code will produce an Abstract Syntax Tree (AST) representing the structure of the C-like code. The AST will be printed to the console as a list of expressions, showing the variable declarations, assignments, if statements, while loops, function declarations, and function calls in the code.

## Contributing

Contributions are welcome! If you find any issues or have suggestions for improvement, feel free to create a pull request or submit an issue.
