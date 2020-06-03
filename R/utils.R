#' Replace Parts of a Function Body
#'
#' Replace quoted targets in the body of a function with quoted replacements.
#'
#' @param fn_body The body of a function (as found via body(fun)).
#' @param target A quoted expression to replace.
#' @param replacement A quoted expression with which the target should be
#'   replaced.
#'
#' @return A function body with the target replaced anywhere it occurs.
#' @export
#'
#' @examples
#' fun <- function(x) {
#'   x^exp
#' }
#' body_replace(body(fun), quote(exp), quote(!!exp))
body_replace <- function(fn_body, target, replacement) {
  if (!is.null(fn_body) && fn_body == target) {
    return(replacement)
  } else if (length(fn_body) > 1) {
    # Break it down into pieces, and run each through fn_replace.
    for (i in seq_along(fn_body)) {
      # Replacing an existing NULL with NULL removes that part of the body.
      # Instead skip it if it's NULL.
      if (!is.null(fn_body[[i]])) {
        fn_body[[i]] <- body_replace(fn_body[[i]], target, replacement)
      }
    }
  }
  return(fn_body)
}

#' Insert Into a Function Body
#'
#' Insert quoted insertions at the start of a function body (after the opening
#' of the function).
#'
#' @param fn_body The body of a function (as found via body(fun)).
#' @param insertion A quoted expression to add at the beginning of the function.
#'
#' @return A function body with the insertion. Note: If before is specified and
#'   is not found anywhere in fn_body, fn_body is returned unaltered.
#' @export
#'
#' @examples
#' fun <- function(x) {
#'   x + 1
#' }
#' body_insert(body(fun), quote(x + 2))
body_insert <- function(fn_body, insertion, before = NULL) {
  if (is.null(before)) {
    if (fn_body[[1]] == quote(`{`)) {
      # fn_body[[1]] will be {. Everything after that has to bump forward 1, and
      # then replace fn_body[[2]] with the insertion.
      fn_body <- .body_insert_location(fn_body, insertion, 2)
    } else {
      stop("Please wrap your function in {}.")
    }
  } else {
    # Find the level where before occurs. Insert insertion on that level, before
    # `before`. Which feels like it should be recursive. If I don't find
    # `before` I return fn_body unaltered.
    if (length(fn_body) > 1) {
      if (any(as.list(fn_body) == before)) {
        # Do the insertion. Things before `before` are unchanged, things after
        # `before` should move 1 higher in the list, and then the spot occupied
        # by before should become insertion.
        target_location <- which(as.list(fn_body) == before)
        fn_body <- .body_insert_location(fn_body, insertion, target_location)
      } else {
        # Check each element of fn_body.
        for (i in seq_along(fn_body)) {
          fn_body[[i]] <- body_insert(fn_body[[i]], insertion, before)
        }
      }
    }
  }

  return(fn_body)
}

.body_insert_location <- function(fn_body, insertion, target_location) {
  for (i in rev(seq_along(fn_body))) {
    if (i >= target_location) {
      fn_body[[i + 1]] <- fn_body[[i]]
    }
  }
  fn_body[[target_location]] <- insertion
  return(fn_body)
}
