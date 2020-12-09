#' @details
#' Wrapper for Oceanbolt API (https://openapi.oceanbolt.com/)
#'
#' @keywords internal
#'
#' @importFrom data.table setDT
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET POST add_headers http_error content timeout
"_PACKAGE"

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

  # Presets package specific options
  options(oceanbolt.base_url = "https://beta.api.oceanbolt.com/v2")

  # Checks if API token was already registered with 'keyring'
  if (requireNamespace("keyring", quietly = TRUE)) {
    keys <- suppressWarnings(keyring::key_list("oceanbolt"))
    if ("oceanbolt" %in% keys$service) {
      token <- suppressWarnings(keyring::key_get("oceanbolt"))
      options(oceanbolt.token = token)
    }
  }

  return(invisible(NULL))
}
