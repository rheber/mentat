# README

Mentat is a simple program for practicing mental arithmetic.

## Building

Run `ghc mentat`.

## Usage

Running `mentat` without any flags will produce five three-digit addition
problems by default.

### Flags

`-l` The minimum value of each operand, 100 by default

`-o` The type of each problem, currently one of: add, sub, mul, square

`-r` How many problems to generate

`-u` The maximum value of each operand, 999 by default

Funny things can happen if you specify your minimum higher than your maximum or
vice versa.