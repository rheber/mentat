# README

Mentat is a simple program for practicing mental arithmetic.

## Building

Run `ghc mentat`.

## Usage

Running `mentat` without any flags will produce five addition problems by default.

### Flags

`-l` The minimum value of each operand, 1 by default

`-o` The type of each problem, currently one of: add, sub, mul, square, tt (times table mode, where each problem involves the maximum value)

`-r` How many problems to generate

`-u` The maximum value of each operand, 999 by default

