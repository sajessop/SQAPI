#' Helper to Make an HTTP Request to the SQUIDLE API Endpoint
#'
#' Internal helper that sends a GET request to the SQUIDLE API endpoint. Adds headers as needed depending on verb.
#'
#' @param verb A valid http verb.
#' @param url A character string representing the full URL for the API export request.
#' @param token A character string containing the authentication token to include in the request header.
#' @param body A named R list to include as the JSON body of the request (for POST or PATCH). Can be \code{NULL}.
#'
#' @return An \code{httr::response} object.
#'
#' @details
#' This function is used internally within request to make API requests.
#'
#' @keywords internal
make_request <- function(verb, url, token, body = NULL) {
  # For POST or PATCH
  if (!is.null(body)) {
    # Ensure body is a valid JSON object for POST/PATCH
    body <- jsonlite::toJSON(body, auto_unbox = TRUE)
    # Make the request using httr::VERB
    response <- httr::VERB(
      verb = verb,
      url = url,
      body = body,
      httr::accept("application/json"),
      httr::content_type("application/json"),
      config = httr::add_headers(
        "x-auth-token" =  token
      )
    )
  } else {
  # For GET requests Make the request using httr::VERB
  # Add relevant headers
  response <- httr::VERB(
    #httr::verbose(data_out = TRUE, data_in = TRUE, info = TRUE, ssl = TRUE),
    verb = verb,
    url = url,
    body = body,
    httr::accept("text/html"),
    httr::content_type("text/html"),
    config = httr::add_headers(
      "x-auth-token" =  token
    )
  )
}
  return(response)
}




#' Make a Request to SQUIDLE API
#'
#' Sends an HTTP request (e.g., GET, POST, PATCH, DELETE) to a specified SQUIDLE API endpoint.
#' Constructs the request URL with query filters and query parameters directly from the output of \code{SQAPI::query_filter()}
#' and \code{SQAPI::query_params}. Also includes authentication headers.
#'
#' @param verb A character string specifying the HTTP verb to use (e.g., `"GET"`, `"POST"`, `"PATCH"`, `"DELETE"`).
#' @param api An instance of \code{SQAPI}, containing properties `"host"` and `"auth"`.
#' @param endpoint A character string specifying the endpoint. See
#' "https://squidle.org/api/help?template=api_help_page.html" for endpoint details.
#' @param query_filters The output from \code{SQAPI::query_filter()}. A named list of filters.
#'  These are appended to the url inside of the \code{q={}} json string.
#' @param query_parameters The output from \code{SQAPI::query_params()}. A list of two elements:
#'   \describe{
#'     \item{\code{q}}{A list of query parameters (e.g., \code{limit}, \code{offset}, \code{order_by}, \code{group_by}, \code{single})
#'       to be encoded as JSON within the \code{q={}} string.}
#'     \item{\code{qparams}}{A list of top-level query parameters (e.g., \code{include_columns}, \code{page}, \code{results_per_page})
#'     that appear outside the \code{q={}} string.}
#'   }
#' @param template Optional character string to specify the template to use (e.g., "data.csv"). Default is JSON.
#' @param disposition Optional character string to specify content disposition. Accepts \code{"attachment"} and \code{"inline"}.
#' See SQUIDLE API documentation for details on template and disposition.
#' @param transform Binary option to append f= query sting as per API GUI. Placeholder and testing tool.
#' @param body A named R list to include as the JSON body of the request (for POST or PATCH). Can be \code{NULL}.
#' \code{NULL} is expected for `"GET"` requests.
#'
#' @return An \code{httr::response} object containing the response from SQUIDLE.
#'
#' @examplesIf interactive()
#'
#' # Example 1: Full GET request example
#'   # Create instance of SQAPI
#' api <- SQAPI$new()
#'   # Create filters
#' my_filters <- query_filter(name = "annotation_set_id", op = "eq", val = "5432")
#'   # Create other parameters
#' my_params <- query_params(page = 14, results_per_page = 56)
#'   # Append filters and parameters and send request
#' r <- request("GET", api, "api/annotation", my_filters, my_params)
#'
#' # Example 2: POST example
#'   # Create instance of SQAPI
#' api <- SQAPI$new()
#'   # Create named list to POST
#' post_me <- list(
#'   "name" = "API test 01",
#'   "description" = "Testing API-created media_collection"
#' )
#'   # Send request with body attached
#' post <- request(verb = "POST", api = api, endpoint = "api/media_collection", body = post_me)
#'
#' # Example 3: PATCH example
#'   # TIP: Test your search query (q) on a GET request first to see what is returned before updating.
#' patch_me <- list(
#' "name" = "API test 02",
#' "description" = "Testing API- media_collection"
#' )
#'   # ID of item to patch is in endpoint (See SQUIDLE API documentation for more examples)
#' patch <- request("PATCH", api, "api/media_collection/14125", body = patch_me)
#'
#' # Example 4: DELETE example
#'   # TIP: Test your search query (q) on a GET request first to see what is returned before updating.
#'   # ID of item to patch is in endpoint (See SQUIDLE API documentation for more examples)
#' patch <- request("DELETE", api, "api/media_collection/14125")
#' @export
request <- function(verb,
                    api,
                    endpoint,
                    query_filters = NULL,
                    query_parameters = NULL,
                    template = NULL,
                    disposition = NULL,
                    transform = FALSE,
                    body = NULL) {
  # Validate input
  if (!inherits(api, "SQAPI"))
    stop("`api` must be an instance of SQAPI.")
  if (!is.character(endpoint))
    stop("`endpoint` must be a character string. See SQUIDLE API documentation for valid endpoints")
  valid_verbs <- c("GET", "POST", "PATCH", "DELETE")
  if (!(toupper(verb) %in% valid_verbs)) {
    stop("Invalid HTTP verb: ", verb)
  }
  if (grepl("export", endpoint, ignore.case = TRUE)) {
    stop("This is an export endpoint. Use SQAPI::export() for export endpoints")
  }


  # Construct and print url
  url <- append_url(
    api = api,
    endpoint = endpoint,
    query_filters = query_filters,
    query_parameters = query_parameters,
    template = template,
    disposition = disposition,
    transform = transform
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
