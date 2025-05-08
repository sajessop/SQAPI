#' Make a Request to SQUIDLE API Export Endpoint
#'
#' Sends an HTTP request to a specified SQUIDLE API export endpoint, with option for status polling and file write to disk.
#' It constructs the request URL with query filters and query parameters directly from the output of \code{SQAPI::query_filter()}
#' and \code{SQAPI::query_params}. It also includes authentication headers. It then initiates the export request, polls
#' the status url, and returns the result. Metadata headers are also written to a separate file if exporting CSV.
#'
#' @param verb A character string specifying the HTTP verb to use (`"GET"`).
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
#'   verb = "GET",
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
#'   query_parameters = my_params_2,
#'   verb = "GET"
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
#'   query_parameters = my_params_3,
#'   verb = "GET"
#' )
#'}
#'
#' @export
export <- function(verb,
                   api,
                   endpoint,
                   query_filters = NULL,
                   query_parameters = NULL,
                   poll = TRUE,
                   write_disk = FALSE,
                   filename = NULL,
                   metadata_filename = "metadata.json") {
  # Helper function to make an API request
  make_export_request <- function(url,
                                  verb,
                                  token,
                                  write_to_disk = FALSE,
                                  file = NULL) {
    if (write_to_disk) {
      return(
        httr::VERB(
          verb = verb,
          url = url,
          config = httr::add_headers("x-auth-token" =  token),
          httr::write_disk(file, overwrite = TRUE)
        )
      )
    } else {
      return(
        httr::VERB(
          verb = verb,
          url = url,
          config = httr::add_headers("x-auth-token" =  token),
          httr::write_memory()
        )
      )
    }
  }


  # Construct and print URL
  url <- append_url(
    api = api,
    endpoint = endpoint,
    query_filters = query_filters,
    query_parameters = query_parameters
  )
  cat("Constructed URL: ")
  cat(utils::URLdecode(url), "\n")

  # Retrieve token
  token <- api$auth

  # Handle filename error
  if (write_disk && is.null(filename)) {
    stop("Error: 'write_disk' is TRUE, but 'filename' is not provided.")
  }

  # Check verb input
  if (!toupper(verb) %in% c("GET", "POST", "PUT", "DELETE", "PATCH")) {
    stop("Unsupported HTTP verb: ", verb)
  }

  # Initial request
  response <- make_export_request(url, verb, token, write_disk, filename)

  # Extract JSON response
  json <- jsonlite::fromJSON(
    httr::content(response, 'text', encoding = "UTF-8"),
    simplifyVector = TRUE,
    flatten = TRUE
  )

  if (poll) {
    # Polling
    results_response <- NULL

    # Define URLs
    host <- paste(api$host)
    status_url <- paste0(host, json$status_url)
    result_url <- paste0(host, json$result_url)

    print(json$message)
    print(paste0("Status URL: ", status_url))
    print(paste0("Results URL: ", result_url))

    # Progress Bar
    pbar <- utils::txtProgressBar(min = 0,
                           max = 100,
                           style = 3)

    while (TRUE) {
      # Check status
      status_response <- httr::VERB(
        verb = verb,
        url = status_url,
        config = httr::add_headers("x-auth-token" =  token)
      )

      json_status_response <- jsonlite::fromJSON(
        httr::content(status_response, 'text', encoding = "UTF-8"),
        simplifyVector = TRUE,
        flatten = TRUE
      )

      # If the result is ready
      if (json_status_response$result_available ||
          json_status_response$status == "done") {
        # Call helper function to handle the write_disk logic and make request to result url
        results_response <- make_export_request(
          url = result_url,
          verb = verb,
          token = token,
          write_to_disk = write_disk,
          file = filename
        )

        utils::setTxtProgressBar(pbar, 100)  # Set progress to 100% when done
        break
      } else if (json_status_response$status == "error") {
        close(pbar)
        stop("Error in processing the request.")
      }

      # Calculate progress
      stages <- json_status_response$progress
      total_iterations <- sum(sapply(stages, function(stage)
        if (!is.null(stage$iteration_count))
          stage$iteration_count
        else
          0))
      completed_iterations <- sum(sapply(stages, function(stage)
        if (!is.null(stage$iteration))
          stage$iteration
        else
          0))

      overall_progress <- if (total_iterations > 0)
        (completed_iterations / total_iterations) * 100
      else
        0
      utils::setTxtProgressBar(pbar, overall_progress)

      Sys.sleep(1)  # Avoid excessive polling
    }

    close(pbar)

    # Retrieve final response
    ret_results <- results_response
  } else {
    ret_results <- response
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

    meta <- ret_results$headers$`x-content-metadata`
    if (!is.null(meta)) {
      writeLines(meta, metadata_filename)
      cat("See metadata file: ", metadata_filename, "\n")
    }
  }

  # Print file write details
  if (write_disk) {
    cat("File downloaded to:", filename, "\n")
  }


  cat("\nResponse Status Code: ", response$status_code, "\n")
  return(ret_results)
}
