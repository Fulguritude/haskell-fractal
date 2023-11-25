# Revision history for hs-fractal



## 1.0.0.0 -- 2023-11-25

Implemented a bona fide fractal render.

Features:
  - works with 4 different R²-algebras (Pairwise, Complex, Dual, Perplex; configurable in the code: look for `Geom`)
  - naive iteration protocol (all pixels)
  - only the Mandelbrot dwell protocol
  - grayscale palette
  - anchor and spread configurable in the code


## 0.1.0.1 -- 2023-11-10

Converted project structure to stack, after a lot of pain, got it to work with VSCode/VSCodium's HLS plugin.


## 0.1.0.0 -- 2023-11-06

Implemented a dumb "Hello World" cabal-based graphics project, which renders a 2D color gradient to window with Gloss.
