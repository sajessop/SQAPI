#' Make a GET Request to SQUIDLE API Export Endpoint
#'
#' Sends a GET request to a specified SQUIDLE API export endpoint, with option for status polling and file write to disk.
#' It constructs the request URL with query filters and query parameters directly from the output of \code{SQAPI::query_filter()}
#' and \code{SQAPI::query_params}. It also includes authentication headers. It then initiates the export request, polls
#' the status url, and returns the result. Metadata headers are also written to a separate file if exporting CSV.
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
#' See SQUIDLE API documentation for details on template and disposition.
#' @param poll Logical. If \code{TRUE}, polls the status url until the result is ready. Defaults to \code{TRUE}.
#' @param write_disk Logical. If \code{TRUE}, writes the result to disk. Defaults to \code{FALSE}.
#' @param filename A character string specifying the output filename (required if \code{write_disk = TRUE}).
#' @param metadata_filename A character string specifying the name of the file to save metadata headers to.
#' Only used if a CSV is being downloaded. Defaults to \code{"metadata.json"}.
#'
#' @return An \code{httr::response} object containing the response from SQUIDLE.
#'
#' @details
#' This function is used to export datasets from SQUIDLE API export endpoints. When \code{poll = TRUE},
#' it will repeatedly check the status url until the export is complete, displaying
#' a progress bar in the console.
#'
#' If \code{write_disk = TRUE}, the response will be saved to the specified file, and optionally
#' metadata can be saved separately. If a CSV file is exported, relevant metadata from headers
#' will be extracted and saved.
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
#'   metadata_filename = "my_metadata1.json"
#' )
#'
#' # Example 2 - Paginated request using limit and offset
#'    # Create instance of SQAPI
#' api <- SQAPI$new()
#'    # Create filters
#' my_filters_2 <- query_filter(
#'   name = "events",
#'   op = "any",
#'   val = query_filter(name = "id", op = "is_not_null")
#' )
#'    # Create parameters
#' my_params_2 <- query_params(limit = 100, offset = 20)
#'
#'    # Send request to export endpoint
#' r2 <- export(
#'   api = api,
#'   endpoint = "api/media_collection/13453/export",
#'   query_filters = my_filters_2,
#'   query_parameters = my_params_2
#' )
#'
#' # Example 3 - Export ordered data as .csv
#'  # Create instance of SQAPI
#' api <- SQAPI$new()
#'    # Create filters
#' my_filters_3 <- query_filter(
#'   name = "events",
#'   op = "any",
#'   val = query_filter(name = "id", op = "is_not_null")
#' )
#'    # Create parameters
#' my_params_3 <- query_params(
#'   template = "data.csv",
#'   order_by = c("pose.dep", "asc"),
#'   include_columns = c(
#'     "id", "key", "path_best", "timestamp_start", "path_best_thm",
#'     "pose.timestamp", "pose.lat", "pose.lon", "pose.alt", "pose.dep",
#'     "pose.data", "pose.id", "deployment.key", "deployment.campaign.key",
#'     "deployment.id", "deployment.campaign.id", "event_log"
#'   )
#' )
#'    # Send request to export endpoint
#' r3 <- export(
#'   api = api,
#'   endpoint = "api/media_collection/13453/export",
#'   query_filters = my_filters_3,
#'   query_parameters = my_params_3)
#'
#'    # Send request and write to disk
#' r3_write_disk <- export(
#'  api = api,
#'   endpoint = "api/media_collection/13453/export",
#'   query_filters = my_filters_3,
#'   query_parameters = my_params_3,
#'   write_disk = TRUE,
#'   filename = "media_collection_13453.json",
#'   metadata_filename = "metadata3.json"
#' )
#' )
#'}
#'
#' @export
export <- function(api,
                   endpoint,
                   query_filters = NULL,
                   query_parameters = NULL,
                   template = NULL,
                   disposition = NULL,
                   poll = TRUE,
                   write_disk = FALSE,
                   filename = NULL,
                   metadata_filename = "metadata.json") {

  # Validate input types
  if (!inherits(api, "SQAPI")) stop("`api` must be an instance of SQAPI.")
  if (!is.character(endpoint)) stop("`endpoint` must be a character string. See SQUIDLE API documentation for valid endpoints")
  if (write_disk && (is.null(filename) || !is.character(filename))) stop("`filename` must be provided and must be a character string if `write_disk = TRUE`.")

  # Construct and print URL
  url <- append_url(
    api = api,
    endpoint = endpoint,
    query_filters = query_filters,
    query_parameters = query_parameters,
    template = template,
    disposition = disposition
  )
  cat("Constructed URL: <", utils::URLdecode(url), ">\n")

  # Retrieve token
  token <- api$auth

  # Initial request
  response <- make_export_request(url, token, write_disk, filename)

  # Extract JSON response
  json <- jsonlite::fromJSON(
    httr::content(response, 'text', encoding = "UTF-8"),
    simplifyVector = TRUE,
    flatten = TRUE
  )
  # Early return if poll = FALSE
  if (!poll) {
    message("Response Status Code: ", response$status_code)
    message("Constructed URL: <", utils::URLdecode(url), ">")
    return(response)
  }
  # else, continue with polling logic
  else {
    # Call polling helper function
    ret_results <- poll_for_result(api = api, json = json, write_disk = write_disk, filename = filename)
  }

  # Handle metadata if it's a CSV file
  if (grepl("data.csv|dataframe.csv", url)) {
    # Clean endpoint for filename
    safe_endpoint <- gsub("[^a-zA-Z0-9_]", "_", endpoint)

    # Make metadata_filename based on filename or endpoint
    metadata_filename <- if (!is.null(filename)) {
      paste(filename, metadata_filename, sep = "_")
    } else {
      paste(safe_endpoint, metadata_filename, sep = "_")
    }
    # Retrieve metadata from headers
    meta <- ret_results$headers$`x-content-metadata`
    if (!is.null(meta)) {
      writeLines(meta, metadata_filename)
      message("See metadata file: ", metadata_filename)
    }
  }

  # Print file write details
  if (write_disk) {
    message("File downloaded to:", filename)
  }


  cat("\nResponse Status Code: ", response$status_code)
  return(ret_results)
}

