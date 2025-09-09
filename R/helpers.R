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




