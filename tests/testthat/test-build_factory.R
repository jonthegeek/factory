test_that("factory basics work", {
  y <- 2
  power <- build_factory(
    fun = function(x) {
      x^exponent
    },
    exponent =
    )
  square <- power(y)
  expect_identical(square(2), 4)
  y <- 7
  expect_identical(square(2), 4)
})

test_that("factory errors", {
  expect_error(
    build_factory(
      fun = function(x) {
        x^exponent
      }
    ),
    "You must provide at least one argument to your factory"
  )

  power <- build_factory(
    fun = function(x) {
      x^exponent
    },
    exponent =
    )
  expect_error(
    power(),
    "argument \"exponent\" is missing, with no default"
  )

  power <- build_factory(
    fun = function(x) {
      x^exponent
    },
    exponent = 2
  )
  expect_error(
    power(),
    NA
  )
})

test_that("Equals unnecessary for arguments.", {
  overpower <- build_factory(
    fun = function(x) {
      x^exponent^other
    },
    exponent,
    other =
    )
  square_cube <- overpower(2, 3)
  expect_identical(square_cube(2), 2^2^3)
})

test_that("NULL default arguments work.", {
  null_ok <- build_factory(
    fun = function(x) {
      c(x, to_add)
    },
    to_add = NULL
  )
  add_null <- null_ok()
  expect_identical(add_null("a"), "a")
  add_a <- null_ok("a")
  expect_identical(add_a("b"), c("b", "a"))
})

test_that("dots [...] as arguments work.", {
  dots_ok <- build_factory(
    fun = function(x, ...) {
      x + y + sum(...)
    },
    y
  )

  add_one <- dots_ok(1)
  expect_identical(add_one(2,3,4), 10)
  expect_setequal(formalArgs(add_one), c("x", "..."))
  expect_identical(as.character(body(add_one)[-1]), "x + 1 + sum(...)")
})