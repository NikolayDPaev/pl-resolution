# pl-resolution
Implementation of the resolution method in first order predicate logic.

## Running
The algorithm runs in static web page.
  - ```elm make src/Main.elm --output main.js --optimize ```
  - open ```index.html``` in browser

## Description
The app takes as input the language symbols as well as set of formulas.
Then processes the formulas and transforms them into disjunct set, that later can be used as input to the resolution method algorithm.
If the formulas are unsatisfiable, the steps for proving that with the resolution method are displayed.

The resolution method is semi-computable which means that it will go in infinite loop on some inputs.

Moreover the displayed steps are optimal in length because the algorithm uses A* for the searching.

## State of the project
Still in progress.

## Known issues:
  - The algorithm runs synchronously to the UI and this can lead to freezing of the whole page if the input formulas are satisfiable.
  - The algorithm fails to start on some inputs
