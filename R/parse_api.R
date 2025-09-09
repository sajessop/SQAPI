#' Parse SQUIDLE API Response
#'
#' Parses the response content from the SQUIDLE API. Attempts to retrieve file type from headers, otherwise falls back to json or user input file type.
#' Supports CSV, JSON, HTML, and TXT.
#'
#' @param response An \code{httr::response} object returned by a request or export call.
#' @param filetype Optional string specifying the expected response format. Defaults to automated extraction of file type from headers, or falls back to \code{"json"}.
#'   Supported values are \code{"csv"}, \code{"json"}, \code{"html"}, and \code{"txt"}.
#' @param view_html Logical; if \code{TRUE} and \code{filetype = "html"}, opens the HTML in the default browser.
#'
#' @return A parsed R object:
#'   \describe{
#'     \item{csv}{A \code{data.frame}.}
#'     \item{json}{A list or \code{data.frame}, depending on the JSON structure.}
#'     \item{html}{Character string of HTML text, or invisibly a file path if \code{view_html = TRUE}.}
#'     \item{txt}{A \code{data.frame}.}
#'   }
#'
#' @details
#' This function converts raw API responses into usable R data structures:
#'   \describe{
#'     \item{csv}{Parses CSV content using \code{read.csv()}.}
#'     \item{json}{Parses JSON content using \code{jsonlite::fromJSON()}.}
#'     \item{html}{Returns HTML text, or opens a temporary file if \code{view_html = TRUE}.}
#'     \item{txt}{Parses tab-delimited content using \code{read.delim()}.}
#'   }
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
#' p1 <- parse_api(r1, "csv")
#' }
#'
#' @export
parse_api <- function(response, filetype = NULL, view_html = FALSE) {
  # If filetype = NULL, attempt to extract the file type from the response
  if (is.null(filetype)) {
    filetype <- get_filetype(response)
  } else {
    filetype <- match.arg(filetype, c("json", "csv", "html", "txt"))
  }

  # Validate auto detected file type
  if (!filetype %in% c("json", "csv", "html", "txt")) {
    warning("Detected filetype '", filetype, "' not supported; defaulting to 'json'. Or manually input file type in function call")
    filetype <- "json"
  }

  parsed <- switch(filetype,
                   "csv" = {
                     raw_text <- rawToChar(response$content)
                     tc <- textConnection(raw_text)
                     on.exit(close(tc))
                     utils::read.csv(tc)
                   },
                   "json" = {
                     jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
                   },
                   "html" = {
                     if (view_html) {
                       html_file <- tempfile(fileext = ".html")
                       writeBin(response$content, html_file)
                       utils::browseURL(html_file)
                       return(invisible(html_file))
                     } else {
                       rawToChar(response$content)
                     }
                   },
                   "txt" = {
                     raw_text <- rawToChar(response$content)
                     tc <- textConnection(raw_text)
                     on.exit(close(tc))
                     utils::read.delim(tc, sep = "\t")
                   },
                   stop("Unsupported filetype")
  )

  return(parsed)
}

