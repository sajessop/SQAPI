#' Search a List and Return a Value
#'
#' Helper function to recursively search a nested list for a key and return the associated value.
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
#' @export
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
#' Concatenates a host URL and an endpoint string into a single URL.
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
#' @export
base_url <- function(host, endpoint){
  url <- paste0(host, "/", endpoint)
  return(url)
}
