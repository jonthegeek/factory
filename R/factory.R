#' Easily Build Function Factories
#'
#' @param fun An anonymous function to turn into a factory.
#' @param ... Arguments for the factory function. If the argument should not
#'   have a default, use the somewhat strange construction \code{a =}. This also
#'   applies for \code{...}, so \code{... =}. Things on the RHS will be
#'   evaluated before building your factory unless explicitly quoted with
#'   \code{quote}. See examples.
#'
#' @return A function factory.
#' @export
#'
#' @examples
#' y <- 2
#' power <- factory(
#'   fun = function(x) {
#'     x^exp
#'   },
#'   exp =
#'   )
#' square <- power(y)
#' square(2)
#' y <- 7
#' square(2)
factory <- function(
                    fun,
                    ...) {
  dots <- rlang::dots_list(..., .ignore_empty = "all", .preserve_empty = TRUE)

  # There should always be at least one dot, or the factory won't do anything.
  if (length(dots) == 0) {
    stop("You must provide at least one argument to your factory.")
  }

  # If their factory takes dots, we need to not check for it in the missing
  # call.
  dot_names <- names(dots)[names(dots) != "..."]
  # For each member of dot_names, we need to walk through the body of the
  # function, and replace dot_names[[n]] with !!dot_names[[n]]. For example,
  # if dot_names[[n]] is exp, we replace exp with !!exp.
  for (dot in dot_names) {
    body(fun) <- body_replace(
      fn_body = body(fun),
      target = rlang::as_name(dot),
      replacement = as.call(list(as.name("!!"), as.name(dot)))
    )
  }

  rlang::new_function(
    args = rlang::exprs(!!!dots),
    body = rlang::expr({
      rlang::new_function(
        !!formals(fun),
        rlang::expr(!!body(fun)),
        rlang::caller_env()
      )
    }),
    env = rlang::caller_env()
  )
}
