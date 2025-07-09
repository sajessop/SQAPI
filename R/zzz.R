.onAttach <- function(libname, pkgname) {
  if (.Platform$OS.type == "windows" &&
      Sys.getenv("CURL_SSL_BACKEND") != "openssl") {
    packageStartupMessage(
      "Note: On Windows, deafult SSL backend is schannel.\n",
      "This may cause SSL/TLS timeout issues with squidle api.\n",
      "To fix, restart R and run this *before* loading any packages:\n",
      "  Sys.setenv(CURL_SSL_BACKEND = 'openssl')"
    )
  }
}
