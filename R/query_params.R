#' Construct Query Parameters for SQUIDLE API Requests
#'
#' Constructs a list of parameters formatted specifically to be used in a SQUIDLE API request. Formats parameters
#' to be passed directly to \code{SQAPI::request()} or \code{SQAPI::export()}. See
#' "https://squidle.org/api/help?template=api_help_page.html" for details on each parameter.
#'
#' @param template Optional character string specifying a response template.
#' @param disposition Optional character string for the response disposition.
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
#'     \item{\code{q}} {The first element is a list of JSON-encoded query parameters (limit, offset, order_by, group_by, single).
#'    These parameters will exist within the \code{q={}} json string in the final url.}
#'     \item{\code{qparams}} {The second element is a list of top-level query parameters (template, disposition, include_columns, page, results_per_page).
#'    These parameters will exist outside of the \code{q={}} json string in the final url.}
#'   }
#'
#' @examples
#' # Example 1: Simple pagination parameters
#' query_params(page = 14, results_per_page = 56)
#'
#' # Example 2: Using grouping and included columns
#' query_params(
#'   template = "data.csv",
#'   group_by = "pose.dep",
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
#'   template = "data.csv",
#'   order_by = c("pose.dep", "asc"),
#'   include_columns = c("id", "pose.dep")
#' )
#'
#' @export
query_params <- function(template = NULL,
                         disposition = NULL,
                         include_columns = NULL,
                         page = NULL,
                         results_per_page = NULL,
                         limit = NULL,
                         offset = NULL,
                         order_by = NULL,
                         group_by = NULL,
                         single = FALSE) {
  qparams <- list()
  q <- list()

  # Append to q list (parameters inside the q={} json string)

  # Handle 'order_by' as a character vector (e.g., c("field1", "asc"))
  json_order_by <- if (!is.null(order_by)) {
    # Check if order_by is a character vector with exactly two elements (field and direction)
    if (length(order_by) != 2 || !is.character(order_by)) {
      stop(
        "ERROR: order_by must be a character vector with exactly 2 elements, e.g., c('<fieldname>', '<order>')"
      )
    }
    order_by <- list(list(field = order_by[1], direction = order_by[2]))
    jsonlite::toJSON(order_by, auto_unbox = TRUE)
  } else {
    NULL
  }
  if (!is.null(json_order_by)) {
    q$order_by <- json_order_by
  }

  # Handle 'group_by' as a single field or a vector of fields
  json_group_by <- if (!is.null(group_by)) {
    if (is.character(group_by)) {
      # If it's a single field (string), make it a list
      group_by <- list(list(field = group_by))
    } else if (is.character(group_by) && length(group_by) > 1) {
      # If it's a vector of fields, process each field
      group_by <- lapply(group_by, function(x)
        list(field = x))
    } else {
      stop("ERROR: group_by must be a string or a character vector of field names.")
    }
    jsonlite::toJSON(group_by, auto_unbox = TRUE)
  } else {
    NULL
  }
  if (!is.null(json_group_by)) {
    q$group_by <- json_group_by
  }

  # Handle others
  if (!is.null(limit)) {
    q$limit <- limit
  }
  if (!is.null(offset)) {
    q$offset <- offset
  }
  if (!is.null(single) && single) {
    q$single <- TRUE
  }

  # Append to qparams list (parameters outside the q={} json string)
  # Handle include columns vector and convert to json
  json_include_columns <- if (!is.null(include_columns)) {
    jsonlite::toJSON(include_columns, auto_unbox = TRUE)
  } else {
    NULL
  }
  if (!is.null(json_include_columns)) {
    qparams$include_columns <- json_include_columns
  }

  # Add other parameters to the qparams list if not NULL
  for (param in c("template", "disposition", "page", "results_per_page")) {
    if (!is.null(get(param)))
      qparams[[param]] <- get(param)
  }
  return(list(q = q, qparams = qparams))
}
