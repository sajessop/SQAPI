#' Parse SQUIDLE API Response
#'
#' Parses the response content from the SQUIDLE API based on the specified template type
#' in \code{query_params} or the default JSON. Supports CSV and JSON response formats.
#'
#' @param response An \code{httr::response} object returned by the request or export call.
#' @param query_parameters Optional. The output from \code{SQAPI::query_params()}. It is used to detect the desired output template.
#' If \code{template = "data.csv"}, the response is parsed as CSV. If \code{template = "dataframe.json"},
#' the response is parsed as a JSON dataframe. If no template is specified or query_papameters is not passed to the function,
#' JSON is assumed by default. A list of two elements:
#'   \itemize{
#'     \item {The first element is a list of JSON-encoded query parameters (limit, offset, order_by, group_by, single).
#'    These parameters will exist within the \code{q={}} json string in the final url.}
#'     \item {The second element is A list of top-level query parameters (template, disposition, include_columns, page, results_per_page).
#'    These parameters will exist outside of the \code{q={}} json string in the final url.}
#'   }
#'
#'
#' @return A parsed R object: a \code{data.frame} for CSV or a list/dataframe for JSON, depending
#'         on the content type.
#'
#' @details
#' This function is used to convert raw API responses into usable R data
#' structures based on the type of template requested. It currently supports:
#' \itemize{
#'   \item \code{"data.csv"}: Converts raw content into a \code{data.frame} using \code{read.csv()}.
#'   \item \code{"dataframe.json"}: Parses JSON content into R using \code{jsonlite::fromJSON()}.
#' }
#'
#' @examples
#' \dontrun{
#' # Example 1 - Export data as .csv with metadata
#'    # Create instance of SQAPI
#' api <- SQAPI$new()
#'    # Create filters
#' my_filters_1 <- query_filter(
#'   name = "events",
#'   op = "any",
#'   val = query_filter(name = "id", op = "is_not_null")
#' )
#'    # Create parameters
#' my_params_1 <- query_params(
#'   template = "data.csv",
#'   group_by = "pose.dep",
#'   include_columns = c(
#'     "id", "key", "path_best", "timestamp_start", "path_best_thm",
#'     "pose.timestamp", "pose.lat", "pose.lon", "pose.alt", "pose.dep",
#'     "pose.data", "pose.id", "deployment.key", "deployment.campaign.key",
#'     "deployment.id", "deployment.campaign.id", "event_log"
#'   )
#' )
#'    # Send request to export endpoint
#' r1 <- export(
#'   api = api,
#'   endpoint = "api/media_collection/13453/export",
#'   query_filters = my_filters_1,
#'   query_parameters = my_params_1,
#'   verb = "GET",
#'   metadata_filename = "my_metadata1.json"
#' )
#'    # Parse
#' p1 <- parse(r1, my_params_1)
#' }
#'
#' @export
parse <- function(response, query_parameters = NULL) {
  template <- if (is.null(query_params)) {
    "default"
  } else {
    # Finds value associated with key "template"
    found <- find_key(query_params, "template")
    if (is.null(found))
      "default"
    else
      found
  }

  parsed <- switch(template,
                   "data.csv" = {
                     raw_text <- rawToChar(response$content)
                     utils::read.csv(textConnection(raw_text))
                   },
                   "dataframe.json" = {
                     jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
                   },
                   {
                     "default" =
                       {
                         jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
                       }
                   })

  return(parsed)
}
