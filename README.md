# haskell-fractal

## Description

In a nutshell, this is a parametric renderer of `A[X]`, the set of polynomials over `A`, where `A` is some 2D algebra (complex numbers, perplex numbers, etc).

The core of the exercise is to render various escape-time fractals with the following constraints:
  - render a fractal image to a window (forces one to learn about available libraries, build systems, and some IO),
  - can work with multiple R²-algebras over arbitrary polynomials (forces a minimum of typed architecture, and gives fun renders),
  - can run various dwell protocols and various rendering algorithm (forces one to implement modular code),
  - can zoom up to machine precision,
  - implements a version of the Mariana-Silver algorithm (to learn how to recurse in the language).

For now, only steps 1, 2, 3 and 5 of this core are done.

## Running

Runs with `stack run`. Constant params in code need to be changed for different renders at this point.
