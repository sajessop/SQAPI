.onAttach <- function(libname, pkgname) {
  if (.Platform$OS.type == "windows") {
    ssl_backend <- tryCatch(
      curl::curl_version()$ssl_version,
      error = function(e) NA
    )
    if (!is.na(ssl_backend) && !grepl("OpenSSL", ssl_backend)) {
      packageStartupMessage(
        "Note: On Windows, you appear to be using the '", ssl_backend, "' SSL backend.\n",
        "This may cause SSL/TLS timeout issues.\n",
        "To fix, restart R and run this *before* loading any packages:\n",
        "  Sys.setenv(CURL_SSL_BACKEND = 'openssl')"
      )
    }
  }
}
