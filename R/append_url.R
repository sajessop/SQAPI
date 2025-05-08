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
#'     \item {The first element is a list of JSON-encoded query parameters (limit, offset, order_by, group_by, single).
#'    These parameters will exist within the \code{q={}} json string in the final url.}
#'     \item {The second element is A list of top-level query parameters (template, disposition, include_columns, page, results_per_page).
#'    These parameters will exist outside of the \code{q={}} json string in the final url.}
#'   }
#'
#' @return A character string representing the full URL encoded to meet the SQUIDLE API requirements.
#'
append_url <- function(api,
                       endpoint,
                       query_filters = NULL,
                       query_parameters = NULL) {
  host <- api$host
  if (is.null(query_filters)) return(base_url(host, endpoint))

  # Wrap filters in a list
  filters <- list(filters = list(query_filters))

  # Initialise q list
  q <- list()

  # Process query parameters
  if (!is.null(query_parameters[[1]])) {
    q <- lapply(names(query_parameters[[1]]), function(param) {
      value <- query_parameters[[1]][[param]]
      if (!is.null(value)) {
        switch(param,
               "order_by" = jsonlite::fromJSON(value, simplifyDataFrame = FALSE),
               "group_by" = jsonlite::fromJSON(value, simplifyDataFrame = FALSE),
               value)
      }
    })
    names(q) <- names(query_parameters[[1]])
    q <- q[!sapply(q, is.null)]  # Remove NULL values
  }

  # Combine filters and q, then convert to JSON
  combined_q_json <- jsonlite::toJSON(c(filters, q), auto_unbox = TRUE)

  # Extract qparams (if they exist)
  qparams <- if (length(query_parameters) > 1) query_parameters[[2]] else list()

  # Construct URL
  url <- httr::parse_url(base_url(host, endpoint))

  # Append filters and parameters
  url$query <- c(list(q = combined_q_json), qparams)

  return(httr::build_url(url))
}
