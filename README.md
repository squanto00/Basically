
# BASICALLY

A modern BASIC interpreter written in BASIC.

BASICALLY is an experimental programming language project focused on
structured BASIC syntax, simplicity, and eventually creating small 2D games.

## Current Status

Early development. BASICALLY 0.7 introduces `WHILE` / `WEND` loops,
building on BASICALLY 0.6's conditional branching with `IF`, `ELSEIF`,
and `ELSE`.

### Implemented Features

- Reading and executing BASIC source files
- `PRINT` statements for text and variable values
- String and integer variables
- Variable declarations using `DIM`
- Integer arithmetic: addition, subtraction, multiplication, and division
- Arithmetic expressions with parentheses and operator precedence
- Unary positive and negative operators
- Comparison operators: `=`, `<>`, `<`, `>`, `<=`, and `>=`
- `IF` / `END IF` conditional blocks
- `ELSE` alternative branches
- `ELSEIF` conditional branches
- Nested conditional blocks
- `WHILE` / `WEND` loops
- Nested loops
- Conditional branching inside loops
- Validation for unmatched `WHILE` and `WEND` statements
- Apostrophe comments and blank lines
- Basic error handling for invalid conditions and division by zero

## Example

```basic
DIM playerHealth AS INTEGER
DIM playerGold AS INTEGER

playerHealth = 75
playerGold = 0

IF playerHealth > 100 THEN
    PRINT "Expert player!"
ELSEIF playerHealth > 0 THEN
    PRINT "Player is alive!"

    IF playerGold > 0 THEN
        PRINT "Player has gold!"
    ELSE
        PRINT "Player needs more gold."
    END IF

ELSE
    PRINT "Game over!"
END IF
```

## Loop Example

```basic
DIM count AS INTEGER

count = 1

WHILE count <= 5
    PRINT count
    count = count + 1
WEND
```

Expected output:

```text
1
2
3
4
5
Program finished.
```

## Requirements

- Linux
- FreeBASIC compiler (`fbc`)

## Building

Compile the interpreter:

```bash
fbc basically.bas -x basically
```

Run a BASICALLY program:

```bash
./basically your-program.bas
```

## Running Tests

The `tests/` directory contains BASIC programs for testing interpreter
features.

Run the regression tests:

```bash
./basically tests/else.bas
./basically tests/elseif.bas
./basically tests/nested-conditions.bas
./basically tests/while.bas
./basically tests/nested-while.bas
./basically tests/while-conditions.bas
./basically tests/unmatched-while.bas
./basically tests/unmatched-wend.bas
```

The unmatched-loop tests should report errors rather than execute normally.

## Planned Features

- Functions and reusable procedures
- Improved error messages and syntax validation
- Self-hosting

## Contributing

BASICALLY is an experimental project, and contributions, bug reports,
feature ideas, and testing are welcome.

See [CONTRIBUTING.md](CONTRIBUTING.md) for contribution guidelines.

## License

BASICALLY is released under the MIT License.
