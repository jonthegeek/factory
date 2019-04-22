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
#'   x ^ exp
#' }
#' body_replace(body(fun), quote(exp), quote(!!exp))
body_replace <- function(fn_body, target, replacement) {
  if (fn_body == target) {
    return(replacement)
  } else if (length(fn_body) > 1) {
    # Break it down into pieces, and run each through fn_replace.
    for(i in seq_along(fn_body)) {
      fn_body[[i]] <- body_replace(fn_body[[i]], target, replacement)
    }
  }
  return(fn_body)
}
