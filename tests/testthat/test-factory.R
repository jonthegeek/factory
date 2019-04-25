test_that("factory basics work", {
  y <- 2
  power <- factory(
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
    factory(
      fun = function(x) {
        x^exp
      }
    ),
    "You must provide at least one argument to your factory"
  )

  power <- factory(
    fun = function(x) {
      x^exp
    },
    exp =
    )
  expect_error(
    power(),
    "argument \"exp\" is missing, with no default"
  )

  power <- factory(
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
