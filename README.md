# pl-resolution
Implementation of the resolution method in first order predicate logic. 
 
[Github pages link](https://nikolaydpaev.github.io/pl-resolution/)

## Running locally
The algorithm runs in static web page.
  - ```elm make src/Main.elm --output main.js --optimize ```
  - open ```index.html``` in browser

## Description
The app takes as input the language symbols as well as set of formulas.
Then processes the formulas and transforms them into disjunct set, that later can be used as input to the resolution method algorithm.
If the formulas are unsatisfiable, the steps for proving that with the resolution method are displayed.

The resolution method is semi-computable, this is why the steps are limited so the algorithm will not go in infinite loop.

Moreover the displayed steps are optimal in length because the algorithm uses A* for the searching.

## State of the project
Still in progress.

## Known issues:
  - Currently the parser is right associative and does not support operation priorities.
  - The algorithm fails to start on some inputs
