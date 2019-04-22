#' Easily Build Function Factories
#'
#' @param fun An anonymous function to turn into a factory.
#' @param ... Arguments for the factory function. If the argument should not
#'   have a default, use the somewhat strange construction \code{a =}. This also
#'   applies for \code{...}, so \code{... =}. Things on the RHS will be
#'   evaluated before building your factory unless explicitly quoted with
#'   \code{quote}. See examples.
#' @param .error_message An optional error message for the factory to use when a
#'   required argument is not provided.
#'
#' @return A function factory.
#' @export
#'
#' @examples
#' y <- 3
#' power <- factory(\{
#'   function(x) \{
#'     x ^ exp
#'   \}
#' \},
#' exp =,
#' .error_message = "Please provide an exponent for your function as exp.")
#' square <- power()
#' square(2)
#' square(y)
#' y <- 7
#' square(y)
factory <- function(
  fun,
  ...,
  .error_message = "One or more arguments to this factory are missing."
) {
  dots <- rlang::dots_list(..., .ignore_empty = "all", .preserve_empty = TRUE)

  # There should always be at least one dot, or the factory won't do anything.
  if (length(dots) == 0) {
    stop("You must provide at least one argument to your factory.")
  }

  # If their factory takes dots, we need to not check for it in the missing
  # call.
  dot_names <- names(dots)[names(dots) != "..."]
  if (length(dot_names) > 0) {
    missing_dots_error <- rlang::expr({
      if (any(missing(!!dot_names))) {
        stop(!!.error_message)
      }
    })

    # For each member of dot_names, we need to walk through the body of the
    # function, and replace dot_names[[n]] with !!dot_names[[n]]. For example,
    # if dot_names[[n]] is exp, we replace exp with !!exp.
    for (dot in dot_names) {
      body(fun) <- body_replace(
        fn_body = body(fun),
        target = rlang::as_name(dot),
        replacement = eval(parse(
            text = paste0("quote(!!", dot, ")")
          ))
      )
    }
  } else {
    # No non-dot arguments, so no error. Right now this puts a "NULL" in the
    # code that doesn't seem to do anything. Ideally this should put literally
    # nothing in the code.
    missing_dots_error <- NULL
  }

  rlang::new_function(
    args = rlang::exprs(!!!dots),
    body = rlang::expr({
      !!missing_dots_error
      rlang::new_function(
        !!formals(fun),
        rlang::expr(!!body(fun)),
        rlang::caller_env()
      )
    }),
    env = rlang::caller_env()
  )
}
