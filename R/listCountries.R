#' Retrieves all the countries that are available in the tradeflow database
#'
#' @param ... Extra parameters passed to \code{\link{listEntities}()}
#'
#' @details
#' See \url{https://openapi.oceanbolt.com/#operation/listCountries} for details.
#'
#' @return Data.table with available countries
#'
#' @examples
#' \dontrun{
#' countries <- listCountries()
#' countries
#' }
#'
#' @export
listCountries <- function(...) {
  listEntities("countries", ...)
}
