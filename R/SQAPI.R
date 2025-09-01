#' SQAPI class for interacting with the SQUIDLE API
#'
#' @description
#' Class SQAPI has a host and auth. Designed to be passes to the \code{api}
#' field throughout SQAPI functions to handle API authentication.
#'
#' @export
SQAPI <- R6::R6Class("SQAPI", public = list(
  #' @field host Host url - defaults to "https://squidle.org".
  host = NULL,
  #' @field auth Authentication for SQUIDLE API account (your API token).
  #' Will prompt for token interactively if NULL
  auth = NULL,

  #' @description
  #' Creates a new instance of SQAPI that has a host and auth. Assign it a
  #' variable name and pass it to the \code{api} field in SQAPI functions such
  #' as \code{export} and \code{request}. This will handle authentication
  #' of all request to SQUIDLE API in an "r" session.
  #' @param host Host url - defaults to "https://squidle.org".
  #' @param auth Your SQUIDLE API token.
  #' @return A new `SQAPI` object.
  initialize = function(host = NULL, auth = NULL) {
    self$host <- if (is.null(host)) "https://squidle.org" else host

    if (!is.null(auth)) {
      self$auth <- auth
    } else if (interactive()) {
      self$auth <- getPass::getPass("Enter your API token: ")
    } else {
      # Fallback for non-interactive mode (e.g., devtools::check())
      self$auth <- "default_password"
    }
  }
))
