#' Make a Request to SQUIDLE API
#'
#' Sends an HTTP request (e.g., GET, POST, PATCH) to a specified SQUIDLE API endpoint.
#' It constructs the request URL with query filters and query parameters directly from the output of \code{SQAPI::query_filter()}
#' and \code{SQAPI::query_params}. It also includes authentication headers.
#' It returns the full response object.
#'
#' @param verb A character string specifying the HTTP verb to use (e.g., `"GET"`, `"POST"`, `"PATCH"`, `"DELETE"`).
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
#' @param body A named R list to include as the JSON body of the request (for POST or PATCH). Can be \code{NULL}.
#' \code{NULL} is expected for `"GET"` requests.
#'
#' @return An \code{httr::response} object containing the response from SQUIDLE.
#'
#' @examples
#' # Example 1: Full GET request example
#'   # Create instance of SQAPI
#' api <- SQAPI$new()
#'   # Create filters
#' my_filters <- query_filter(name = "annotation_set_id", op = "eq", val = "5432")
#'   # Create other parameters
#' my_params <- query_params(page = "14", results_per_page = "56")
#'   # Append filters and parameters and send request
#' r <- request("GET", api, "api/annotation", my_filters, my_params)
#'
#' # Example 2: POST example
#'   # Create instance of SQAPI
#' api <- SQAPI$new()
#'   # Create named list to POST
#' post_me <- list(
#'   "name" = "API test 01",
#'   "description" = "Testing API-created media_collection",
#'   "user_id"= 007
#' )
#'   # Send request with body attached
#' post <- request(verb = "POST", api = api, endpoint = "api/media_collection", body = post_me)
#'
#' @export
request <- function(verb,
                    api,
                    endpoint,
                    query_filters = NULL,
                    query_parameters = NULL,
                    body = NULL) {

  # Helper function to make request handling get/post/patch logic
  make_request <- function(verb, url, token, body = NULL) {
    # Handle body for POST or PATCH requests
    if (!is.null(body)) {
      # Ensure body is a valid JSON object for POST/PATCH
      body <- jsonlite::toJSON(body, auto_unbox = TRUE)
    }

    # Print statement
    #print(paste("Body for", verb, "request: ", body))

    # Make the request using httr::VERB
    response <- httr::VERB(
      verb = verb,
      url = url,
      body = body,
      config = httr::add_headers(
        "x-auth-token" =  token,
        "Content-Type" = "application/json",
        "Accept" = "application/json"
      )
    )

    return(response)
  }

  # Construct and print url
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

  # Make request using helper function
  response <- make_request(verb, url, token, body)

  # Print status
  cat("Response Status Code: ", response$status_code, "\n")

  # Print response object
  print(response)
  return(response)
}
