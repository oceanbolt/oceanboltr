#' Retrives list of available segments in database
#'
#' @param ... Extra parameters passed to \code{\link{listEntities}()}
#'
#' @details
#' See \url{https://openapi.oceanbolt.com/#operation/listSegments} for details.
#'
#' @return Data.table with available segments
#'
#' @examples
#' \dontrun{
#' segments <- listSegments()
#' segments
#' }
#'
#' @export
listSegments <- function(...) {
  listEntities("segments", ...)
}
