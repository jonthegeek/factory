test_that("factory basics work", {
  y <- 2
  power <- build_factory(
    fun = function(x) {
      x^exp
    },
    exp =
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
        x^exp
      }
    ),
    "You must provide at least one argument to your factory"
  )

  power <- build_factory(
    fun = function(x) {
      x^exp
    },
    exp =
    )
  expect_error(
    power(),
    "argument \"exp\" is missing, with no default"
  )

  power <- build_factory(
    fun = function(x) {
      x^exp
    },
    exp = 2
  )
  expect_error(
    power(),
    NA
  )
})

test_that("Equals unnecessary for arguments.", {
  overpower <- build_factory(
    fun = function(x) {
      x^exp^other
    },
    exp,
    other =
  )
  square_cube <- overpower(2, 3)
  expect_identical(square_cube(2), 2^2^3)

  power <- build_factory(
    fun = function(x) {
      x^exponent
    },
    exponent
  )
  square <- power(2)
  expect_identical(square(2), 4)

  power <- build_factory(
    fun = function(x) {
      x^exponent
    },
    exponent =
  )
  square <- power(2)
  expect_identical(square(2), 4)

  power <- build_factory(
    fun = function(x) {
      x^exponent
    },
    exponent = 2
  )
  square <- power()
  expect_identical(square(2), 4)

  power <- build_factory(
    fun = function(x) {
      x^exponent
    },
    exponent = quote(power_default)
  )
  power_default <- 2
  square <- power()
  power_default <- 3
  expect_identical(square(2), 4)
})
