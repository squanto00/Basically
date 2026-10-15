# BASICALLY

A modern BASIC interpreter written in BASIC.

BASICALLY is an experimental programming language project focused on
structured BASIC syntax, simplicity, and eventually creating small 2D games.

## Current Status

Early development.

Currently implemented:
- Reading BASIC source files
- Basic PRINT statements
- Basic string literal handling
- Initial string variable support

Planned features:
- Numeric variables and arithmetic
- IF / ELSE statements
- Loops
- Functions
- Self-hosting
- A lightweight 2D game engine
- Built-in graphics and sound tools

## Requirements

- Linux
- FreeBASIC compiler (`fbc`)

## Building

Compile the interpreter:

```bash
fbc basically.bas -x basically
