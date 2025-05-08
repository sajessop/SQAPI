#' Create class "SQAPI" with property "host" and "auth"
#'
#' @description
#' Class SQAPI has a host and authorisation.
#' @export
SQAPI <- R6::R6Class("SQAPI", public = list(
  #' @field host Host url - defaults to "https://squidle.org".
  host = NULL,
  #' @field auth Authorisation for your SQUIDLE API account (your API token).
  auth = NULL,

  #' @description
  #' Creates a new instance of SQAPI that has a host and authorisation.This will handle authorisation of all request to SQUIDLE API in an "r" session.
  #' @param host Host url - defaults to "https://squidle.org".
  #' @param auth Your SQUIDLE API token.
  #' @return A new `SQAPI` object.
  initialize = function(host = NULL) {
    if (is.null(host)) {
      self$host <- "https://squidle.org"
    } else{
      self$host <- host
    }

    if (is.null(getOption("api_token"))) {
      if (interactive()) {
        self$auth <- getPass::getPass("Enter your API token: ")
      } else {
        # Fallback for non-interactive (e.g. devtools::check()
        self$auth <- "default_password"
      }
      options(api_token = self$auth)
    } else {
      self$auth <- getOption("api_token")
    }
  }
))
