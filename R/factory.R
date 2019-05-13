#' Easily Build Function Factories
#'
#' @param fun An anonymous function to turn into a factory.
#' @param ... Arguments for the factory function. Things on the RHS will be
#'   evaluated before building your factory unless explicitly quoted with
#'   \code{quote}. See examples.
#'
#' @return A function factory.
#' @export
#'
#' @examples
#' y <- 2
#' power <- build_factory(
#'   fun = function(x) {
#'     x^exponent
#'   },
#'   exponent
#' )
#' square <- power(y)
#' square(2)
#' y <- 7
#' square(2)
build_factory <- function(
                    fun,
                    ...) {
  # To deal with *all* possibilities, we need to enquo the dots (so nobody tries
  # to evaluate them).
  dots <- rlang::enquos(...)
  dot_names <- names(dots)

  # There should always be at least one dot, or the factory won't do anything.
  if (length(dot_names) == 0) {
    stop("You must provide at least one argument to your factory.")
  }

  # I used to allow for (and then deal with) ..., but I don't think it makes
  # sense for the factory to accept .... If we need that, I need to sort out how
  # to make it work; it wasn't actually working how I expected before, which I
  # discovered via actually trying to test it.

  # Start args as dots, but it will evolve as we go.
  args <- as.list(dots)

  for (i in seq_along(dot_names)) {
    dot_name <- dot_names[[i]]
    dot_value <- dots[[i]]

    # If the name is "", the thing in dot_value is actually meant to be the
    # name.
    if (dot_name == "") {
      dot_name <- rlang::as_name(dot_value)
      names(args)[[i]] <- dot_name
      args[[i]] <- rlang::missing_arg()
    } else if (rlang::is_missing(rlang::quo_get_expr(args[[i]]))) {
      # Next we need to deal with the case where they passed in "arg =" like they
      # might expect they're supposed to do.
      args[[i]] <- rlang::missing_arg()
    } else {
      # In all other cases we need to deal with the actual value passed in. We
      # evaluate it unless they explicitly quote it.
      args[[i]] <- rlang::eval_tidy(args[[i]])
    }

    # For each member of dot_names, we need to walk through the body of the
    # function, and replace dot_names[[n]] with !!dot_names[[n]]. For example,
    # if dot_names[[n]] is exp, we replace exp with !!exp.
    body(fun) <- body_replace(
      fn_body = body(fun),
      target = dot_name,
      replacement = as.call(list(as.name("!!"), as.name(dot_name)))
    )
  }

  # Need to get the args argument working again. I think above, when we're
  # processing dot by dot, we need to build an alist. Need to be careful to
  # differentiate between a NULL default and no default.

  rlang::new_function(
    args = args,
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
