% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/request.R
\name{request}
\alias{request}
\title{Make a Request to SQUIDLE API}
\usage{
request(
  verb,
  api,
  endpoint,
  query_filters = NULL,
  query_parameters = NULL,
  template = NULL,
  disposition = NULL,
  transform = FALSE,
  body = NULL
)
}
\arguments{
\item{verb}{A character string specifying the HTTP verb to use (e.g., \code{"GET"}, \code{"POST"}, \code{"PATCH"}, \code{"DELETE"}).}

\item{api}{An instance of \code{SQAPI}, containing properties \code{"host"} and \code{"auth"}.}

\item{endpoint}{A character string specifying the endpoint. See
"https://squidle.org/api/help?template=api_help_page.html" for endpoint details.}

\item{query_filters}{The output from \code{SQAPI::query_filter()}. A named list of filters.
These are appended to the url inside of the \code{q={}} json string.}

\item{query_parameters}{The output from \code{SQAPI::query_params()}. A list of two elements:
\itemize{
\item{\code{q}} {A list of query parameters (e.g., \code{limit}, \code{offset}, \code{order_by}, \code{group_by}, \code{single})
to be encoded as JSON within the \code{q={}} string.}
\item{\code{qparams}} {A list of top-level query parameters (e.g., \code{include_columns}, \code{page}, \code{results_per_page})
that appear outside the \code{q={}} string.}
}}

\item{template}{Optional character string to specify the template to use (e.g., "data.csv"). Default is JSON.}

\item{disposition}{Optional character string to specify content disposition. Accepts \code{"attachment"} and \code{"inline"}.
See SQUIDLE API documentation for details on template and disposition.}

\item{transform}{Binary option to append f= query sting as per API GUI. Placeholder and testing tool.}

\item{body}{A named R list to include as the JSON body of the request (for POST or PATCH). Can be \code{NULL}.
\code{NULL} is expected for \code{"GET"} requests.}
}
\value{
An \code{httr::response} object containing the response from SQUIDLE.
}
\description{
Sends an HTTP request (e.g., GET, POST, PATCH, DELETE) to a specified SQUIDLE API endpoint.
Constructs the request URL with query filters and query parameters directly from the output of \code{SQAPI::query_filter()}
and \code{SQAPI::query_params}. Also includes authentication headers.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}

# Example 1: Full GET request example
  # Create instance of SQAPI
api <- SQAPI$new()
  # Create filters
my_filters <- query_filter(name = "annotation_set_id", op = "eq", val = "5432")
  # Create other parameters
my_params <- query_params(page = 14, results_per_page = 56)
  # Append filters and parameters and send request
r <- request("GET", api, "api/annotation", my_filters, my_params)

# Example 2: POST example
  # Create instance of SQAPI
api <- SQAPI$new()
  # Create named list to POST
post_me <- list(
  "name" = "API test 01",
  "description" = "Testing API-created media_collection"
)
  # Send request with body attached
post <- request(verb = "POST", api = api, endpoint = "api/media_collection", body = post_me)

# Example 3: PATCH example
  # TIP: Test your search query (q) on a GET request first to see what is returned before updating.
patch_me <- list(
"name" = "API test 02",
"description" = "Testing API- media_collection"
)
  # ID of item to patch is in endpoint (See SQUIDLE API documentation for more examples)
patch <- request("PATCH", api, "api/media_collection/14125", body = patch_me)

# Example 4: DELETE example
  # TIP: Test your search query (q) on a GET request first to see what is returned before updating.
  # ID of item to patch is in endpoint (See SQUIDLE API documentation for more examples)
patch <- request("DELETE", api, "api/media_collection/14125")
\dontshow{\}) # examplesIf}
}
