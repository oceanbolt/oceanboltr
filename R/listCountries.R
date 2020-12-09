#' Retrieves all the countries that are available in the tradeflow database
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
listCountries <- function() {
  listEntities("countries")
}
