#' Search a List and Return a Value
#'
#' Internal helper to recursively search a nested list for a key and return the associated value.
#'
#' @param lst A list to search. Can be nested.
#' @param key A character string representing the key to search for.
#'
#' @return The value associated with the first occurrence of the key, or \code{NULL} if the key is not found.
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
#'
base_url <- function(host, endpoint){
  url <- paste0(host, "/", endpoint)
  return(url)
}

#' Search for file type in response headers
#'
#' Internal helper that searches for file type in response headers.
#'
#' @param response Response object from a request or export call.
#'
#' @return String representing file type.
#'
#'
get_filetype <- function(response) {
  if (!is.null(response$headers$`content-disposition`)) {
    ext <- sub(".*\\.([a-zA-Z0-9]+)$", "\\1", response$headers$`content-disposition`)
    return(tolower(ext))
  } else {
    return("json")
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


