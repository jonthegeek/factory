
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
# install.packages("remotes")
remotes::install_github("jonthegeek/factory")
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

### The Simplest Factories are Fragile

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
square function by calling `power1` with `exponent = 2`.

``` r
square1 <- power1(2)
square1(2)
#> [1] 4
# 2 ^ 2 = 4
square1(3)
#> [1] 9
# 3 ^ 2 = 9
```

However, `power1` is fragile. Let’s think about what the definition of
power1 *means.* The function returned by `power1` raises its argument to
whatever the `exponent` variable is defined as. Let’s see what happens
if we use a variable in the global environment to define our `square`
function.

``` r
my_exponent <- 2
square1a <- power1(my_exponent)
```

Due to R’s lazy evaluation, when we call `power1`, the `exponent`
variable gets a promise to take on the value of the `my_exponent`
variable. But `my_exponent` doesn’t actually have the value of `2` yet.
Until we *use* `my_exponent`, it has a *promise* to get the value of
`2`. If we call `square1a` right away, it works as expected.

``` r
square1a(2)
#> [1] 4
# 2 ^ 2 = 4
my_exponent <- 3
square1a(3)
#> [1] 9
# 3 ^ 2 = 9
```

The `my_exponent` promise (which was passed in during the definition of
`square1a`) resolves to `2` the first time it is needed (when `square1a`
is first called). After that initial call, that is the value used in
`square1a` forever.

But if `my_exponent` changes between definition of our function and
first call of that function, we get a different result.

``` r
my_exponent <- 2
square1b <- power1(my_exponent)
my_exponent <- 3
square1b(2)
#> [1] 8
# 2 ^ 3 = 8
square1b(3)
#> [1] 27
# 3 ^ 3 = 27
```

What happened? When `square1b` was defined, `my_exponent` was passed in
as a *promise.* However, before `my_exponent` was ever actually *used*,
its value changed. The promise isn’t evaluated *until it is used,*
which, in this case, is the first time `square1b` is called. Once the
promise is evaluated, its value is “fixed,” and the function works as
expected.

### Forcing Arguments Trades Fragility for Complexity

We can make factories that are less fragile, if we remember to `force`
the variables.

``` r
power2 <- function(exponent) {
  force(exponent) # Gah, easy to forget!
  function(x) {
    x ^ exponent
  }
}

my_exponent <- 2
square2 <- power2(my_exponent)
my_exponent <- 3
square2(2)
#> [1] 4
# 2 ^ 2 = 4
square2(3)
#> [1] 9
# 3 ^ 2 = 9
```

Why does this work? The `force` function forces the evaluation of its
argument. We don’t really need to use `force`, per se. Any function that
forces evaluation would work, but `force` makes it obvious why we’re
doing it. For example, we could produce the same result by `message`ing
within the factory.

``` r
power2b <- function(exponent) {
  message("The exponent's value is ", exponent)
  function(x) {
    x ^ exponent
  }
}

my_exponent <- 2
square2b <- power2b(my_exponent)
#> The exponent's value is 2
my_exponent <- 3
square2b(2)
#> [1] 4
# 2 ^ 2 = 4
square2b(3)
#> [1] 9
# 3 ^ 2 = 9
```

Since the value of `exponent` is needed for the message, the promise is
evaluated when the factory is invoked, and the resulting function is
stable.

While such factories are more stable, it’s easy to miss a `force`. And,
in both of these cases, the resulting functions are difficult to
understand as a user.

``` r
square1
#> function(x) {
#>     x ^ exponent
#>   }
#> <environment: 0x000000001499b588>
square2
#> function(x) {
#>     x ^ exponent
#>   }
#> <environment: 0x000000001d0daba8>
cube <- power2(3)
cube
#> function(x) {
#>     x ^ exponent
#>   }
#> <bytecode: 0x000000001db4af70>
#> <environment: 0x000000001da4d950>
```

It isn’t clear what these functions will do, since the definitions of
`exponent` are hidden inside the function environments.

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
thus easier for users to understand.

``` r
square3 <- power3(2)
square3
#> function (x) 
#> {
#>     x^2
#> }
```

The {rlang} calls are very difficult to understand, though. It would be
nice to get the stability and interpretability of the rlang-produced
functions, with the ease-of-programming of the simplest function
factories.

## Enter {factory}

The goal of `factory` is to make function factories as straightforward
to create as in `power1`, but to make the resulting functions make as
much sense as in `power3`. Right now, the calls are still a *little*
more complicated than I would like, but they’re definitely easier to
understand than the {rlang} calls.

``` r
library(factory)
power4 <- build_factory(
  fun = function(x) {
    x ^ exponent
  },
  exponent
)

my_exponent <- 2
square4 <- power4(my_exponent)
my_exponent <- 3
square4(2)
#> [1] 4
# 2 ^ 2 = 4
```

The resulting function makes sense, as with `power3`.

``` r
square4
#> function (x) 
#> {
#>     x^2
#> }
```
