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
#' power <- factory(
#'   fun = function(x) {
#'     x^exp
#'   },
#'   exp
#'   )
#' square <- power(y)
#' square(2)
#' y <- 7
#' square(2)
factory <- function(
                    fun,
                    ...) {
  # To get the args for new_function, we need to use dots_list.
  args <- rlang::dots_list(
    ...,
    .ignore_empty = "all",
    .preserve_empty = TRUE
  )

  dot_names <- names(args)

  # But we also need to know what they literally passed in.
  dots <- rlang::enquos(...)

  # There should always be at least one dot, or the factory won't do anything.
  if (length(dot_names) == 0) {
    stop("You must provide at least one argument to your factory.")
  }

  # I used to allow for (and then deal with) ..., but I don't think it makes
  # sense for the factory to accept .... If we need that, I need to sort out how
  # to make it work; it wasn't actually working how I expected before, which I
  # discovered via actually trying to test it.

  for (i in seq_along(dot_names)) {
    dot <- dot_names[[i]]
    # If any dot_names == "", we need to replace them with the literal value of
    # dots.
    if (dot == "") {
      dot <- rlang::as_name(dots[[i]])
      names(args)[[i]] <- dot
      args[[i]] <- rlang::missing_arg()
    }
    # For each member of dot_names, we need to walk through the body of the
    # function, and replace dot_names[[n]] with !!dot_names[[n]]. For example,
    # if dot_names[[n]] is exp, we replace exp with !!exp.
    body(fun) <- body_replace(
      fn_body = body(fun),
      target = dot,
      replacement = as.call(list(as.name("!!"), as.name(dot)))
    )
  }

  rlang::new_function(
    args = rlang::exprs(!!!args),
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
