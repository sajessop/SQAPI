#' Construct Query Parameters for SQUIDLE API Requests
#'
#' Constructs a list of parameters formatted specifically to be used in a SQUIDLE API request. Formats parameters
#' to be passed directly to \code{SQAPI::request()} or \code{SQAPI::export()}. See
#' "https://squidle.org/api/help?template=api_help_page.html" for details on each parameter.
#'
#' @param include_columns Optional character vector specifying which columns to include in the response.
#' @param page Optional integer specifying the page number for paginated results.
#' @param results_per_page Optional integer for the number of results per page.
#' @param limit Optional integer to specify the maximum number of objects to return.
#' @param offset Optional integer to specify the offset into the result set of the returned list of instances.
#' @param order_by A character vector of length 2 specifying a field and direction, where field is a string corresponding to the name of
#' a field of the requested model and direction is either "asc" for ascending order or "desc" for descending order.
#' @param group_by A character string or character vector specifying one or more fields to group results by.
#' @param single Logical, whether to request a single item (`TRUE`) or multiple (`FALSE`).
#'
#' @return A list of two elements:
#'   \itemize{
#'     \item{\code{q}} {A list of query parameters (e.g., \code{limit}, \code{offset}, \code{order_by}, \code{group_by}, \code{single})
#'       to be encoded as JSON within the \code{q={}} string.}
#'     \item{\code{qparams}} {A list of top-level query parameters (e.g., \code{include_columns}, \code{page}, \code{results_per_page})
#'     that appear outside the \code{q={}} string.}
#'   }
#'
#' @examples
#' # Example 1: Simple pagination parameters
#' query_params(page = 14, results_per_page = 56)
#'
#' # Example 2: Using grouping and included columns
#' query_params(
#'   group_by = c("deployment.campaign.id", "deployment.id"),
#'   include_columns = c(
#'     "id", "key", "path_best", "timestamp_start", "path_best_thm",
#'     "pose.timestamp", "pose.lat", "pose.lon", "pose.alt", "pose.dep",
#'     "pose.data", "pose.id", "deployment.key", "deployment.campaign.key",
#'     "deployment.id", "deployment.campaign.id", "event_log"
#'   )
#' )
#'
#' # Example 3: Ordering results by pose.dep in ascending order
#' query_params(
#'   order_by = c("pose.dep", "asc"),
#'   include_columns = c("id", "pose.dep")
#' )
#'
#' @export
query_params <- function(include_columns = NULL,
                         page = NULL,
                         results_per_page = NULL,
                         limit = NULL,
                         offset = NULL,
                         order_by = NULL,
                         group_by = NULL,
                         single = FALSE) {
  qparams <- list()
  q <- list()

  # Validate numeric inputs
  if (!is.null(page) && !is.numeric(page)) stop("page must be a numeric value.")
  if (!is.null(results_per_page) && !is.numeric(results_per_page)) stop("results_per_page must be numeric.")
  if (!is.null(limit) && !is.numeric(limit)) stop("limit must be numeric.")
  if (!is.null(offset) && !is.numeric(offset)) stop("offset must be numeric.")

  # Handle 'order_by'
  if (!is.null(order_by)) {
    if (length(order_by) != 2 || !is.character(order_by)) {
      stop("ERROR: order_by must be a character vector with exactly 2 elements, e.g., c('<fieldname>', '<order>')")
    }
    if (!order_by[2] %in% c("asc", "desc")) {
      stop("ERROR: order_by direction must be either 'asc' or 'desc'")
    }
    q$order_by <- jsonlite::toJSON(list(list(field = order_by[1], direction = order_by[2])), auto_unbox = TRUE)
  }

  # Handle 'group_by'
  if (!is.null(group_by)) {
    if (!is.character(group_by)) {
      stop("ERROR: group_by must be a character string or a character vector.")
    }
    q$group_by <- jsonlite::toJSON(lapply(group_by, function(f) list(field = f)), auto_unbox = TRUE)
  }

  # Handle other q params
  if (!is.null(limit)) q$limit <- limit
  if (!is.null(offset)) q$offset <- offset
  if (isTRUE(single)) q$single <- TRUE

  # Handle qparams (outside q={})
  if (!is.null(include_columns)) {
    qparams$include_columns <- jsonlite::toJSON(include_columns, auto_unbox = TRUE)
  }
  if (!is.null(page)) qparams$page <- page
  if (!is.null(results_per_page)) qparams$results_per_page <- results_per_page

  return(list(q = q, qparams = qparams))
}
