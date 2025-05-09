#' Helper to Make an HTTP Request to the SQUIDLE API Export Endpoint
#'
#' Internal helper that sends a GET request to the SQUIDLE API export endpoint. It can either
#' write the result to disk or return the content in memory, based on the provided arguments.
#'
#' @param url A character string representing the full URL for the API export request.
#' @param token A character string containing the authentication token to include in the request header.
#' @param write_to_disk Logical; if \code{TRUE}, the response will be written to disk.
#'   If \code{FALSE}, the response will be stored in memory.
#' @param file A character string specifying the filename to save the response to when \code{write_to_disk = TRUE}.
#'   This argument is ignored if \code{write_to_disk = FALSE}.
#'
#' @return An \code{httr::response} object. If \code{write_to_disk = TRUE}, the response is written to disk.
#'   If \code{write_to_disk = FALSE}, the response content is stored in memory.
#'
#' @details
#' This function is used internally within other functions (specifically export) to make API requests. It is flexible in how it handles
#' the response, allowing either a file save to disk or in-memory storage for further processing.
#'
#' @keywords internal
#'
make_export_request <- function(url,
                                token,
                                write_to_disk = FALSE,
                                file = NULL) {
  if (write_to_disk) {
    return(
      httr::GET(
        url = url,
        config = httr::add_headers("x-auth-token" =  token),
        httr::write_disk(file, overwrite = TRUE)
      )
    )
  } else {
    return(
      httr::GET(
        url = url,
        config = httr::add_headers("x-auth-token" =  token),
        httr::write_memory()
      )
    )
  }
}


#' Helper for Status polling logic for SQUIDLE export
#'
#' Internal helper that polls a status URL until the result is ready,
#' then retrieves the final result from the result URL.
#'
#' @param api An instance of \code{SQAPI}, containing properties `"host"` and `"auth"`.
#' @param json The parsed JSON response from the initial export request.
#' @param write_disk Logical; whether to write the result to disk.
#' @param filename Filename to save the result if write_disk is TRUE.
#'
#' @return An httr::response object.
#'
#' @keywords internal
#'
poll_for_result <- function(api, json, write_disk, filename) {
  # Retrieve auth and host from api
  token <- api$auth
  host <- api$host

  # Create and print status and result urls
  status_url <- paste0(host, json$status_url)
  result_url <- paste0(host, json$result_url)

  cat(json$message, "\n")
  cat("Status URL: ", status_url, "\n")
  cat("Results URL: ", result_url, "\n")

  # Progress bar
  pbar <- utils::txtProgressBar(min = 0, max = 100, style = 3)

  while (TRUE) {
    # Send GET request to status url
    status_response <- httr::GET(
      url = status_url,
      config = httr::add_headers("x-auth-token" = token)
    )
    # Check for errors
    httr::stop_for_status(status_response, "poll status url")
    # Parse response content as JSON
    json_status_response <- jsonlite::fromJSON(
      httr::content(status_response, 'text', encoding = "UTF-8"),
      simplifyVector = TRUE,
      flatten = TRUE
    )
    # Check if result is available and make export request if it is
    if (isTRUE(json_status_response$result_available) ||
        identical(json_status_response$status, "done")) {
      result_response <- make_export_request(
        url = result_url,
        token = token,
        write_to_disk = write_disk,
        file = filename
      )
      utils::setTxtProgressBar(pbar, 100)
      break
    }
    # Handle error
    if (identical(json_status_response$status, "error")) {
      close(pbar)
      message(json_status_response$message)
      stop("Error in processing the request.")
    }

    # Calculate progress in stages from status url
    stages <- json_status_response$progress

    total_iterations <- sum(sapply(stages, function(stage) {
      if (!is.null(stage$iteration_count)) stage$iteration_count else 0
    }))

    completed_iterations <- sum(sapply(stages, function(stage) {
      if (!is.null(stage$iteration)) stage$iteration else 0
    }))

    progress <- if (total_iterations > 0) {
      (completed_iterations / total_iterations) * 100
    } else {
      0
    }

    utils::setTxtProgressBar(pbar, progress)
    Sys.sleep(1)
  }

  close(pbar)
  return(result_response)
}

#' Make a GET Request to SQUIDLE API Export Endpoint
#'
#' Sends a GET request to a specified SQUIDLE API export endpoint, with option for status polling and file write to disk.
#' It constructs the request URL with query filters and query parameters directly from the output of \code{SQAPI::query_filter()}
#' and \code{SQAPI::query_params}. It also includes authentication headers. It then initiates the export request, polls
#' the status url, and returns the result. Metadata headers are also written to a separate file if exporting CSV.
#'
#' @param api An instance of \code{SQAPI}, containing properties `"host"` and `"auth"`.
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
#' @return
#' If \code{poll = TRUE}, returns the final \code{httr::response} from the result url after polling the status url.
#' If \code{poll = FALSE}, returns the initial \code{httr::response} from the export request without polling the status url.
#'
#' @details
#' This function is used to export datasets from SQUIDLE API export endpoints. When \code{poll = TRUE},
#' it will repeatedly check the status url until the export is complete, displaying
#' a progress bar in the console. If \code{poll = FALSE}, the function returns immediately after the initial
#' request without checking the status or downloading the result.
#' This is useful for inspecting or debugging the initial export response manually.

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
    cat("Response Status Code: ", response$status_code, "\n")
    message("Initial response returned without polling. To retrieve the final result, set poll = TRUE.")
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

