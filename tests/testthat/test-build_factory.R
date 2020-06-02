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
  expect_identical(add_one(2, 3, 4), 10)
  expect_setequal(formalArgs(add_one), c("x", "..."))
  expect_identical(as.character(body(add_one)[-1]), "x + 1 + sum(...)")
})

test_that("Factories can pass dots.", {
  number_format <- build_factory(
    fun = function(x, ...) {
      scales::number(
        x,
        accuracy = accuracy, scale = scale, prefix = prefix,
        suffix = suffix, big.mark = big.mark, decimal.mark = decimal.mark,
        trim = trim, ...
      )
    },
    accuracy = NULL,
    scale = 1,
    prefix = "",
    suffix = "",
    big.mark = " ",
    decimal.mark = ".",
    trim = TRUE,
    .pass_dots = TRUE
  )

  expect_identical(
    scales::number_format(width = 8)(1:10 * 10000),
    number_format(width = 8)(1:10 * 10000)
  )

  expect_error(
    build_factory(
      fun = function(x) {
        mean(x * multiple)
      },
      multiple,
      .pass_dots = TRUE
    ),
    regexp = "fun must contain"
  )

  number_format2 <- factory::build_factory(
    fun = function(x) {
      scales::number(
        x,
        accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
        big.mark = big.mark, decimal.mark = decimal.mark, trim = trim, ...
      )
    },
    accuracy = NULL,
    scale = 1,
    prefix = "",
    suffix = "",
    big.mark = " ",
    decimal.mark = ".",
    trim = TRUE,
    .pass_dots = TRUE
  )

  expect_identical(
    scales::number_format(width = 8)(1:10 * 10000),
    number_format2(width = 8)(1:10 * 10000)
  )
  expect_identical(
    formals(number_format2(width = 8)),
    as.pairlist(alist(x = ))
  )
})

test_that("Can build factories that branch into functions by variable.", {
  base_bins <- build_factory(
    .internal_variables = list(
      nclass_fun = switch(
        type,
        Sturges = grDevices::nclass.Sturges,
        scott = grDevices::nclass.scott,
        FD = grDevices::nclass.FD,
        stop("Unknown type", call. = FALSE)
      )
    ),
    fun = function(x) {
      (max(x) - min(x)) / nclass_fun(x)
    },
    type
  )
  x <- rnorm(3 * 100, sd = c(1, 5, 15))
  expect_identical(
    base_bins("Sturges")(x),
    (max(x) - min(x)) / nclass.Sturges(x)
  )

  expect_error(
    build_factory(
      .internal_variables = c(nclass_fun = "this"),
      fun = function(x) {
        (max(x) - min(x)) / nclass_fun(x)
      },
      type
    ),
    regexp = "internal_variables must be a named list"
  )

  # I have not finished this test but don't want to delete it.
  # plot_dev <- build_factory(
  #   fun = list(
  #     ext = list(
  #       ps = function(path, ...) {
  #         grDevices::postscript(
  #           file = filename, ..., onefile = FALSE, horizontal = FALSE,
  #           paper = "special"
  #         )
  #       },
  #       png = function(...) {
  #         grDevices::png(..., res = dpi, units = "in")
  #       }
  #     )
  #   ),
  #   ext,
  #   dpi = 96
  # )
})
