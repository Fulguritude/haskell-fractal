# haskell-fractal

This is a reprise of my old 42 "fractol" project. When I need or want to learn a new language, this is the project I do. In a nutshell, this is a parametric renderer of `A[X]`, the set of polynomials over `A`, where `A` is some 2D algebra (complex numbers, perplex numbers, etc).

The core of the exercise is to render various escape-time fractals with the following constraints:
  - render a fractal image to a window (forces one to learn about available libraries, build systems, and some IO),
  - can work with multiple R²-algebras over arbitrary polynomials (forces a minimum of typed architecture, and gives fun renders),
  - can run various dwell protocols and various rendering algorithm (forces one to implement modular code),
  - can zoom up to machine precision,
  - implements a version of the Mariana-Silver algorithm (to learn how to recurse in the language).

It's a project that I've done:
  - in C (core done, with a lot of interactivity, but only for complex numbers),
  - in React/TypeScript (core done, and even some amount of interactivity, though somewhat buggy and not too UX-friendly),
  - in Rust (only a rudimentary implementation, didn't have much time),
  - and now in Haskell (for now, only steps 1, 2, 3 and 5 of the core are done).

Depending on the amount of time that I have before needing to use the language professionally, I put more or less care/features into this project. Other features include:
  - saving to file (as a set of parameters, and/or as a picture), loading from file (set of parameters)
  - keyboard and mouse interactivity
  - in-app changes to the parameters, including, but not limited to:
    - the anchor position
    - the level of zoom
    - coefficients of the iteration polynomial
    - the dwell protocol
    - the rendering algorithm
    - the color palette
    - the color smoothing algorithm
    - the R²-algebra chosen
    - the escape radius
    - show/unshow axes
    - show/unshow Mariani-Silver boundaries

Features that I'd like to do but never got the chance to
  - deep (arbitrary precision) zoom 
  - more Mariani-Silver tilings than just the quadtree version
  - n-dimensional fractals using geometric algebra
  - complex color smoothing algorithms
  - trace the evolving path of a point's iterations when hovering over it
