#' Retrieves all available tonnage zones
#'
#' @param ... Extra parameters passed to \code{\link{listEntities}()}
#'
#' @details
#' See \url{https://openapi.oceanbolt.com/#operation/listTonnageZones} for
#' details.
#'
#' @return Data.table with available tonnage regions
#'
#' @examples
#' \dontrun{
#' zones <- listZones()
#' zones
#' }
#'
#' @export
listZones <- function(...) {
  listEntities("zones", ...)
}
