#' Retrieves a list of all commodities available in the trade flow database
#'
#' @details
#' See \url{https://openapi.oceanbolt.com/#operation/listCommodities} for
#'  details.
#'
#' @return Data.table with available commodities
#'
#' @examples
#' \dontrun{
#' commodities <- listCommodities()
#' commodities
#' }
#'
#' @export
listCommodities <- function() {
  listEntities("commodities")
}
