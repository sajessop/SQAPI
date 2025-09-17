#' Skeleton function to build a transform object for API queries
#'
#' Constructs a `transform` list object to pass to the SQUIDLE API request or export function.
#'
#' @param ... One or more lists, each representing an operation.
#' @return A list of operations suitable for \code{SQAPI::request} or \code{SQAPI::export}
#'
#' @examples
#' # Single operation
#' transform(list(module = "pandas", method = "json_normalize"))
#'
#' @export
transform <- function(...) {
  ops <- list(...)
  if (length(ops) == 0) stop("You must supply at least one operation (as a list).")

  # make sure all inputs are lists
  if (!all(vapply(ops, is.list, logical(1)))) {
    stop("All operations must be lists of key-value pairs.")
  }

  list(operations = ops)
}
