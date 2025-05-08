#' Create a Query Filter for SQUIDLE API Requests
#'
#' Constructs a named list representing a query filter. The filter includes
#' a name, an operator , and a value.
#' Can be used to created nested lists for nested queries. Details on fields and operators
#' can be found at "https://squidle.org/api/help?template=api_help_page.html"
#'
#' @param name A character string specifying the name of the field to filter.
#' @param op A character string specifying the operator (e.g., \code{"=="}, \code{"!="}, \code{"in"}).
#' @param val The value to compare the field against. Can be another filter (i.e., nested).
#'
#' @return A named list representing the filter, to be included in your query.
#'
#' @examples
#' query_filter("id", "eq", "5432")
#'
#' query_filter(
#' name = "media",
#' op = "any",
#' val = query_filter(
#'   name = "deployment",
#'   op = "has",
#'   val = query_filter(
#'    name = "campaign",
#'    op = "has",
#'    val = query_filter(name = "key", op = "eq", val = "Batemans201011")
#'   )
#' )
#' )
#'
#' @export
query_filter <- function(name,
                         op,
                         val = NULL) {
  # Create the filter list and exclude 'val' if NULL
  qfilter <- list(name = name, op = op)
  if (!is.null(val)){
    qfilter$val <- val
  }

  return(qfilter)
}
