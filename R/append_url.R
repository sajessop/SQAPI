#' Construct a full SQUIDLE API URL
#'
#' Constructs a complete URL by appending an endpoint to the API host,
#' optionally adds query filters and query parameters. Filters
#' and parameters are passed to this function directly from the output of \code{SQAPI::query_filter()}
#' and \code{SQAPI::query_params}. Typically used as a helper function in \code{SQAPI::request()} and
#' \code{SQAPI::export()}
#'
#' @param api An instance of SQAPI, containing properties `"host"` and `"auth"`.
#' @param endpoint A character string specifying the endpoint. See
#' "https://squidle.org/api/help?template=api_help_page.html" for endpoint details.
#' @param query_filters The output from \code{SQAPI::query_filter()}. A named list of filters.
#'  These are appended to the url inside of the \code{q={}} json string.
#' @param query_parameters The output from \code{SQAPI::query_params()}. A list of two elements:
#'   \itemize{
#'     \item{\code{q}} {A list of query parameters (e.g., \code{limit}, \code{offset}, \code{order_by}, \code{group_by}, \code{single})
#'       to be encoded as JSON within the \code{q={}} string.}
#'     \item{\code{qparams}} {A list of top-level query parameters (e.g., \code{include_columns}, \code{page}, \code{results_per_page})
#'     that appear outside the \code{q={}} string.}
#'   }
#' @param template Optional character string to specify the template to use (e.g., "data.csv"). Default is JSON.
#' @param disposition Optional character string to specify content disposition. Accepts \code{"attachment"} and \code{"inline"}.
#'
#' @return A character string representing the full URL encoded to meet the SQUIDLE API requirements.
#'
append_url <- function(api,
                       endpoint,
                       query_filters = NULL,
                       query_parameters = NULL,
                       template = NULL,
                       disposition = NULL) {
  # Define host and return host + endpoint if there are no query_filters
  host <- api$host
  if (is.null(query_filters)) return(base_url(host, endpoint))

  # Wrap filters in a list
  filters <- list(filters = list(query_filters))

  # Directly extract q and qparams from query_parameters
  q <- query_parameters$q
  qparams <- query_parameters$qparams
  if (!is.null(template)) qparams$template <- template
  if (!is.null(disposition)) qparams$disposition <- disposition

  # Process q
  processed_q <- list()
  for (param in names(q)) {
    value <- q[[param]]
    if (!is.null(value)) {
      if (param %in% c("order_by", "group_by")) {
        processed_q[[param]] <- jsonlite::fromJSON(value, simplifyDataFrame = FALSE)
      } else {
        processed_q[[param]] <- value
      }
    }
  }

  # Combine filters and processed_q, convert to JSON
  combined_q_json <- jsonlite::toJSON(c(filters, processed_q), auto_unbox = TRUE)

  # Construct URL
  url <- httr::parse_url(base_url(host, endpoint))
  url$query <- c(list(q = combined_q_json), qparams)

  return(httr::build_url(url))
}

