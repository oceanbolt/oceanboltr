#' Retrieves all the regions that are available in the tradeflow database
#'
#' @details
#' See \url{https://openapi.oceanbolt.com/#operation/listRegions} for details.
#'
#' @return Data.table with available regions
#'
#' @examples
#' \dontrun{
#' regions <- listRegions()
#' regions
#' }
#'
#' @export
listRegions <- function() {
  listEntities("regions")
}
