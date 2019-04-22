
<!-- README.md is generated from README.Rmd. Please edit that file -->
factory
=======

<!-- badges: start -->
<!-- badges: end -->
The goal of factory is to make construction of function factories easy. This is very much a work in progress. I'm sure there are easier or better ways to implement this. I mostly wanted to see if I could do it.

Installation
------------

You can install factory from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jonthegeek/factory")
```

Example
-------

Function factories are functions that make functions. They can be confusing to work with. For example, they can produce functions that are fragile (examples from [Advanced R by Hadley Wickham (2nd Edition), 10.2.3: Forcing Evaluation](https://adv-r.hadley.nz/function-factories.html#forcing-evaluation), "Gah" comments are me):

``` r
power1 <- function(exp) {
  function(x) {
    x ^ exp
  }
}

x <- 2
square1 <- power1(x)

x <- 3
square1(2) # Gah, fragile!
#> [1] 8
#> [1] 8
```

You can make factories that are lesss fragile, if you remember to `force` the variables.

``` r
power2 <- function(exp) {
  force(exp) # Gah, easy to forget!
  function(x) {
    x ^ exp
  }
}

x <- 2
square2 <- power2(x)
x <- 3
square2(2)
#> [1] 4
#> [1] 4
```

However, the resulting function can be hard to understand:

``` r
square2
#> function(x) {
#>     x ^ exp
#>   }
#> <environment: 0x0000000012c6ae78>
```

You can make functions that are easier to understand, but building the function factory is much more difficulty (from [Advanced R by Hadley Wickham (2nd Edition), 19.7.4: Creating functions](https://adv-r.hadley.nz/quasiquotation.html#new-function)):

``` r
power3 <- function(exp) {
  rlang::new_function(
    rlang::exprs(x = ), 
    rlang::expr({
      x ^ !!exp
    }), 
    rlang::caller_env()
  )
}
```

The resulting functions simply look like a normal function:

``` r
square3 <- power3(2)
square3
#> function (x) 
#> {
#>     x^2
#> }
```

The goal of `factory` is to make function factories as easy to create as in `power1`, but to make the resulting functions make as much sense as in `power3`:

``` r
library(factory)
power4 <- factory(
  fun = function(x) {
    x ^ exp
  },
  exp = # For the time being, you need to tell factory which arguments belong to the factory.
)

x <- 2
square4 <- power4(x)
x <- 3
square4(2)
#> [1] 4
```

The resulting function is clear, as with power3:

``` r
square4
#> function (x) 
#> {
#>     x^2
#> }
```
