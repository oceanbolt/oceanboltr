#' @details
#' Wrapper for Oceanbolt API (https://openapi.oceanbolt.com/)
#'
#' @keywords internal
#'
#' @importFrom data.table setDT
#' @importFrom jsonlite fromJSON
#' @importFrom httr RETRY add_headers http_error content status_code timeout
"_PACKAGE"

#' Base Oceanbolt API URL
#'
#' @keywords internal
baseApiUrl <- "https://beta.api.oceanbolt.com/v2"

#' Internal function to initialize package options
#'
#' @param libname a character string giving the library directory where
#' the package defining the namespace was found
#' @param pkgname a character string giving the name of the package
#'
#' @keywords internal
#'
#' @return Invisible NULL
.onLoad <- function(libname, pkgname) {

  # Due to NSE notes in R CMD check / devtools::check()
  libname <- pkgname <- NULL

  # Sets package settings
  Sys.setenv("OCEANBOLT_RETRY_TIMEOUT" = 30)
  Sys.setenv("OCEANBOLT_RETRY_TIMES" = 3)
  Sys.setenv("OCEANBOLT_RETRY_PAUSE_BASE" = 10)
  Sys.setenv("OCEANBOLT_RETRY_PAUSE_MIN" = 10)
  Sys.setenv("OCEANBOLT_RETRY_PAUSE_CAP" = 90)

  # Checks if API token was already registered with 'keyring'
  if (requireNamespace("keyring", quietly = TRUE)) {
    keys <- suppressWarnings(keyring::key_list("OCEANBOLT_TOKEN"))
    if ("OCEANBOLT_TOKEN" %in% keys$service) {
      token <- suppressWarnings(keyring::key_get("OCEANBOLT_TOKEN"))
      Sys.setenv("OCEANBOLT_TOKEN" = token)
    }
  }

  invisible(NULL)
}
