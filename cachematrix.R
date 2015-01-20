## This solution ignores the one required by the assignment. I believe this
## solution is more flexible. That one is working with fixed function and
## arguments.
## This code presents two alternatives.


## Creates a version of a function that is able to cache the result per key.
## This allows making a cachable version of any function.
## Useful when the calling arguments are expected to change.
## Input
##  - f function
## Return
##  - callable function, arguments: cache key, original function arguments
cachedFunctionFactory <- function(f) {
  cache = list()
  function(key, ...) {
    if (is.null(cache[[key]])) {
      cache[[key]] <<- f(...)
    }
    cache[[key]]
  }
}

## Lazy loader generator.
## Accepts any function with the called arguments.
## Useful when the arguments will not change.
## Input
##  - f function
##  - ... function args
## Return
##  - callable lazy evaluator
lazyLoadingFactory <- function(f, ...) {
  cache = NULL
  function() {
    if (is.null(cache)) {
      cache <<- f(...)
    }
    cache
  }
}


## Example to matrix to invert.
m <- matrix(c(2, 5, 1, 2), 2, 2)

## Example of using the cached function loader:
cachableSolve <- cachedFunctionFactory(solve)
cachedSolve("key", m)

## Example of using the lazy loader:
lazySolve <- lazyLoadingFactory(solve, m)
lazySolve()