#' Search a List and Return a Value
#'
#' Internal helper to recursively search a nested list for a key and return the associated value.
#'
#' @param lst A list to search. Can be nested.
#' @param key A character string representing the key to search for.
#'
#' @return The value associated with the first occurrence of the key, or \code{NULL} if the key is not found.
#'
#' @examples
#' nested_list <- list(
#'   a = 1,
#'   b = list(
#'     c = 2,
#'     d = list(
#'       e = 3,
#'       target = "found me"
#'     )
#'   )
#' )
#' find_key(nested_list, "target")
#'  # Returns: "found me"
#'
find_key <- function(lst, key) {
  if (is.list(lst)) {
    if (key %in% names(lst)) {
      return(lst[[key]])
    }
    for (sub in lst) {
      result <- find_key(sub, key)
      if (!is.null(result)) return(result)
    }
  }
  return(NULL)
}


#' Construct a Base URL from Host and Endpoint
#'
#' Internal helper that concatenates a host URL and an endpoint string into a single URL.
#'
#' @param host A character string representing the base host URL (e.g., \code{"https://squidle.org"}).
#' @param endpoint A character string representing the API endpoint path (e.g., \code{"api/media_collection"}).
#'
#' @return A character string containing the full URL.
#'
#' @examples
#' base_url("https://squidle.org", "api/media_collection")
#' # Returns: "https://squidle.org/api/media_collection"
#'
base_url <- function(host, endpoint){
  url <- paste0(host, "/", endpoint)
  return(url)
}


#' Make an HTTP Request to the SQUIDLE API Export Endpoint
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
#' @examples
#' # Example usage:
#' response <- make_export_request(
#'   url = "https://example.com/api/export",
#'   token = "your_auth_token",
#'   write_to_disk = TRUE,
#'   file = "exported_data.json"
#' )
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


#' Status polling logic for SQUIDLE export
#'
#' Internal helper that polls a status URL until the result is ready,
#' then retrieves the final result from the result URL.
#'
#' @param api An instance of SQAPI, containing properties `"host"` and `"auth"`.
#' @param json The parsed JSON response from the initial export request.
#' @param write_disk Logical; whether to write the result to disk.
#' @param filename Filename to save the result if write_disk is TRUE.
#'
#' @return An httr::response object.
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

