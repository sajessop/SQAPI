#' Parse SQUIDLE API Response
#'
#' Parses the response content from the SQUIDLE API based on the specified template type
#' in \code{query_params} or the default JSON. Supports CSV and JSON response formats.
#'
#' @param response An \code{httr::response} object returned by the request or export call.
#' @param template Optional string specifying the expected response format.
#'   Defaults to \code{"dataframe.json"}. Supported values are \code{"data.csv"} and \code{"dataframe.json"}.
#'
#' @return A parsed R object:
#' \itemize{
#'   \item If \code{template = "data.csv"}: a \code{data.frame}.
#'   \item If \code{template = "dataframe.json"}: a list or data frame, depending on the JSON structure.
#' }
#'
#' @details
#' This function is used to convert raw API responses into usable R data
#' structures based on the response template used in the request. Supported formats include:
#' \itemize{
#'   \item \code{"data.csv"}: Parses CSV content into a \code{data.frame} using \code{read.csv()}.
#'   \item \code{"dataframe.json"}: Parses JSON content into a list or \code{data.frame} using \code{jsonlite::fromJSON()}.
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
#'   template = "data.csv",
#'   verb = "GET",
#'   metadata_filename = "my_metadata1.json"
#' )
#'    # Parse
#' p1 <- parse(r1, "data.csv")
#' }
#'
#' @export
parse <- function(response, template = NULL) {
  template <- if (is.null(template)) "dataframe.json" else template


  parsed <- switch(template,
                   "data.csv" = {
                     raw_text <- rawToChar(response$content)
                     utils::read.csv(textConnection(raw_text))
                   },
                   "dataframe.json" = {
                     jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
                   })

  return(parsed)
}
