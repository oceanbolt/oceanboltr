#' Retrieves all the ports that are available in the congestion database
#'
#' @param ... Extra parameters passed to \code{\link{listEntities}()}
#'
#' @details
#' See \url{https://openapi.oceanbolt.com/#operation/listCongestionPorts} for
#' details.
#'
#' @return Data.table with available countries
#'
#' @examples
#' \dontrun{
#' ports <- listPorts()
#' ports
#' }
#'
#' @export
listPorts <- function(...) {
  listEntities("ports", ...)
}
