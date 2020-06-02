#' Easily Build Function Factories
#'
#' @param fun A function to turn into a factory.
#' @param ... Arguments for the factory function. Things on the RHS will be
#'   evaluated before building your factory unless explicitly quoted with
#'   \code{quote}. See examples.
#' @param pass_dots A logical indicating whether the factory should accept
#'   additional arguments (...) to pass on to methods. In order for this to
#'   work, the manufactured function *must* also include dots, and the input
#'   \code{fun} must indicate where those dots are used.
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
build_factory <- function(fun,
                          ...,
                          pass_dots = FALSE) {
  if (!...length()) {
    stop("You must provide at least one argument to your factory.")
  }

  # To deal with *all* possibilities, we need to enquos the dots (so nobody
  # tries to evaluate them).
  dots <- rlang::enquos(...)
  dots_names <- names(rlang::quos_auto_name(dots))

  args <- as.list(dots) %>%
    purrr::modify_if(
      ~ (rlang::is_quosure(.) && rlang::quo_is_null(.)),
      ~ rlang::list2(NULL)
    ) %>%
    purrr::modify_if(
      ~ (rlang::is_quosure(.) && rlang::quo_is_missing(.)),
      ~ rlang::list2(rlang::missing_arg())
    ) %>%
    purrr::modify_if(
      names(dots) == "",
      ~ rlang::list2(rlang::missing_arg())
    ) %>%
    purrr::modify_if(
      rlang::is_quosure,
      ~ rlang::list2(rlang::eval_tidy(.))
    ) %>%
    purrr::flatten() %>%
    purrr::set_names(dots_names)

  # We also need to update the function body.
  body(fun) <- purrr::reduce(
    dots_names,
    ~ body_replace(
      fn_body = ..1,
      target = ..2,
      replacement = rlang::call2("!!", rlang::sym(..2))
    ),
    .init = body(fun)
  )

  child_fn <- rlang::expr({
    rlang::new_function(
      args = !!formals(fun),
      body = rlang::expr(!!body(fun)),
      env = rlang::caller_env()
    )
  })

  if (pass_dots) {
    args <- rlang::pairlist2(
      !!!args,
      "..." =
      )
    old_fun <- fun
    body(fun) <- body_insert(
      fn_body = body(fun),
      insertion = quote(!!!dots),
      before = quote(...)
    )
    if (identical(old_fun, fun)) {
      stop("fun must contain ... when pass_dots is TRUE.")
    }

    # Update child_fun.
    child_fn <- rlang::expr({
      rlang::new_function(
        args = !!formals(fun),
        body = rlang::expr(!!body(fun)),
        env = rlang::caller_env()
      )
    }) %>%
      body_insert(
        insertion = quote(dots <- list(...))
      )
  }


  return(
    rlang::new_function(
      args = args,
      body = child_fn,
      env = rlang::caller_env()
    )
  )
}
