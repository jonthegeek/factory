#' Easily Build Function Factories
#'
#' @param fun A function to turn into a factory.
#' @param ... Arguments for the factory function. Things on the RHS will be
#'   evaluated before building your factory unless explicitly quoted with
#'   \code{quote}. See examples.
#' @param .pass_dots A logical indicating whether the factory should accept
#'   additional arguments (...) to pass on to methods. In order for this to
#'   work, the manufactured function *must* also include dots, and the input
#'   \code{fun} must indicate where those dots are used.
#' @param .internal_variables A named list of additional code to run to create
#'   additional variables used by the factory.
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
#'
#' base_bins <- build_factory(
#'   .internal_variables = list(
#'     nclass_fun = switch(
#'       type,
#'       Sturges = nclass.Sturges,
#'       scott = nclass.scott,
#'       FD = nclass.FD,
#'       stop("Unknown type", call. = FALSE)
#'     )
#'   ),
#'   fun = function(x) {
#'     (max(x) - min(x) / nclass_fun(x))
#'   },
#'   type
#' )
#' base_bins("Sturges")
build_factory <- function(fun,
                          ...,
                          .pass_dots = FALSE,
                          .internal_variables = NULL) {
  if (!...length()) {
    stop("You must provide at least one argument to your factory.")
  }

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

  # I can't find a way to do this neatly with rlang. I want the user to pass
  # bare code in a list, and I don't want to evaluate that code. !!! unquotes
  # and thus breaks, so instead I need to enexpr and then pull the result apart.
  to_do <- rlang::enexpr(.internal_variables)
  if (length(to_do)) {
    if (as.character(to_do[[1]]) != "list") {
      stop(".internal_variables must be a named list of code.")
    }
    to_do[[1]] <- NULL

    # We need to catch anything in .internal_variables when we update the
    # function.
    dots_names <- rlang::list2(!!!names(to_do), !!!dots_names)
  }

  # multiple_funs <- rlang::enexpr(fun)
  # return(multiple_funs)

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

  if (.pass_dots) {
    args <- rlang::pairlist2(
      !!!args,
      "..." =
      )
    old_fun <- fun

    # If they want to pass ... to the child, we *add* !!!dots. Else we replace
    # ... with !!!dots.
    if ("..." %in% names(formals(fun))) {
      body(fun) <- body_insert(
        fn_body = body(fun),
        insertion = quote(!!!dots),
        before = quote(...)
      )
    } else {
      body(fun) <- body_replace(
        fn_body = body(fun),
        target = quote(...),
        replacement = quote(!!!dots)
      )
    }

    if (identical(old_fun, fun)) {
      stop("fun must contain ... when .pass_dots is TRUE.")
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

  if (length(to_do)) {
    child_fn <- purrr::reduce2(
      names(to_do),
      to_do,
      ~ body_insert(
        fn_body = ..1,
        insertion = rlang::call2(
          `<-`,
          rlang::sym(..2),
          ..3
        )
      ),
      .init = child_fn
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

# build_factory.list <- function(fun,
#                                ...,
#                                .pass_dots = FALSE,
#                                .internal_variables = NULL) {
#   # The structure of the list is somewhat complicated and strict. Check that
#   # first.
#   if (length(list) != 1) {
#     stop("We can currently only make the function choice based on",
#          " 1 argument.",
#          " If you need something more complex, perhaps try",
#          " .internal_variables.")
#   }
#   control_variable <- names(fun)
#   if (! (control_variable %in% dots_names)) {
#     stop(
#       "For now at least, you must supply the name of the control variable",
#       "in the ... of the build_factory call."
#     )
#   }
#
#   new_call <- rlang::call2(
#     switch,
#     rlang::sym(control_variable),
#     !!!fun[[1]],
#     quote(stop("Unknown value for function chooser."))
#   )
#
#   new_fun <- rlang::new_function(
#     args = args,
#     body = new_call,
#     env = rlang::caller_env()
#   )
#
#   return(
#     build_factory(
#       new_fun,
#       ...,
#       .pass_dots,
#       .internal_variables
#     )
#   )
# }
