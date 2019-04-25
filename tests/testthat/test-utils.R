test_that("body_replace replaces bits of bodies", {
  test_fun <- function(x) {
    x^exp
  }
  body(test_fun) <- body_replace(body(test_fun), quote(exp), quote(!!exp))
  expected_fun <- function(x) {
    x^!!exp
  }
  expect_identical(test_fun, expected_fun)
})
