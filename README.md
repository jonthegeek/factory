
<!-- README.md is generated from README.Rmd. Please edit that file -->

# factory <img src='man/figures/factory.png' align="right" height="138.5" />

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/jonthegeek/factory.svg?branch=master)](https://travis-ci.org/jonthegeek/factory)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/jonthegeek/factory?branch=master&svg=true)](https://ci.appveyor.com/project/jonthegeek/factory)
[![Codecov test
coverage](https://codecov.io/gh/jonthegeek/factory/branch/master/graph/badge.svg)](https://codecov.io/gh/jonthegeek/factory?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/factory)](https://CRAN.R-project.org/package=factory)
<!-- badges: end -->

The goal of factory is to make construction of function factories more
straightforward, without requiring the user to learn the `rlang`
package.

## Installation

Install the released version of factory from CRAN:

``` r
install.packages("factory")
```

Or install the development version from
[GitHub](https://github.com/jonthegeek/factory) with:

``` r
# install.packages("devtools")
devtools::install_github("r-lib/usethis")
```

## Motivation

Function factories are functions that make functions. They can be
confusing to work with. For example, as we’ll see below, they can
produce functions that are fragile, or that are confusing to work with
as a user.

WARNING: All code shown below is “wrong” in some way until we get to the
example at the end\! These examples show the dangers of working with
function factories, and why this package exists.

(examples adapted from [Advanced R by Hadley Wickham (2nd
Edition), 10.2.3: Forcing
Evaluation](https://adv-r.hadley.nz/function-factories.html#forcing-evaluation))

### The Simplest Factories

`power1` is a function factory. It returns a function based on the
`exponent` argument.

``` r
power1 <- function(exponent) {
  function(x) {
    x ^ exponent
  }
}
```

For many use cases, `power1` works fine. For example, we can define a
square function by calling power1 with `exponent = 2`.

``` r
square1 <- power1(2)
square1(2)
#> [1] 4
square1(3)
#> [1] 9
```

However, `power1` is fragile. For example, imagine we defined our square
function by referencing a variable defined in the global environment.

``` r
x <- 2
square1a <- power1(x)
```

If we call `square1a` before `x` changes, it works as expected.

``` r
square1a(2)
#> [1] 4
x <- 3
square1a(3)
#> [1] 9
```

But if `x` changes in-between definition of our function and first call
of that function, we get a different result.

``` r
x <- 2
square1b <- power1(x)
x <- 3
square1b(2)
#> [1] 8
square1b(3)
#> [1] 27
```

This fragility results from `x` being a **promise** in the `square1b`
function environment. Once the promise is evaluated, its value is
“fixed,” and the function works as expected.

### Forcing arguments

We can make factories that are less fragile, if we remember to `force`
the variables.

``` r
power2 <- function(exponent) {
  force(exponent) # Gah, easy to forget!
  function(x) {
    x ^ exponent
  }
}

x <- 2
square2 <- power2(x)
x <- 3
square2(2)
#> [1] 4
square2(3)
#> [1] 9
```

However, the resulting function can be hard to understand:

``` r
square2
#> function(x) {
#>     x ^ exponent
#>   }
#> <environment: 0x0000000014963a28>
```

It isn’t clear what this function will do, since the definition of
`exponent` is hidden inside the function’s environment.

### Using rlang

We can use {rlang} to make functions that are easier to understand, but
building the function factory is much more difficult (from [Advanced R
by Hadley Wickham (2nd Edition), 19.7.4: Creating
functions](https://adv-r.hadley.nz/quasiquotation.html#new-function)):

``` r
power3 <- function(exponent) {
  rlang::new_function(
    rlang::exprs(x = ), 
    rlang::expr({
      x ^ !!exponent
    }), 
    rlang::caller_env()
  )
}
```

The resulting functions look like a “normal” function, though, and are
thus easier for users to understand:

``` r
square3 <- power3(2)
square3
#> function (x) 
#> {
#>     x^2
#> }
```

## Enter {factory}

The goal of `factory` is to make function factories as straightforward
to create as in `power1`, but to make the resulting functions make as
much sense as in `power3`:

``` r
library(factory)
power4 <- build_factory(
  fun = function(x) {
    x ^ exponent
  },
  exponent
)

x <- 2
square4 <- power4(x)
x <- 3
square4(2)
#> [1] 4
```

The resulting function makes sense, as with power3:

``` r
square4
#> function (x) 
#> {
#>     x^2
#> }
```
