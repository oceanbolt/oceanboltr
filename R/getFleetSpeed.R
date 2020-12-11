#' Retrieves fleet speed data for a given list of zone_ids/segments/directions
#'
#' @param zoneId The list of zoneIds to get tonnage data for. ZoneId can be
#' obtained either with \code{\link{listZones}()}, or alternatively from the
#' \url{https://app.oceanbolt.com} interactive zone selector. Defaults to
#' \code{"Baltic Sea"}.
#'
#' @param segment The list of vessel segments for which to get fleet speed data.
#' Allowed values can be obtained with \code{\link{listSegments}()}.
#'
#' @param subSegment The list of vessel sub segments for which to get fleet
#' speed data. Allowed values can be obtained with \code{\link{listSegments}()}.
#'
#' @param direction The list of directions to get fleet speed data for.
#' The following directions are allowed: \code{NNE, ENE, ESE, SSE, SSW, WSW,
#' WNW, NNW}. Directions can also be obtained from the interactive direction
#' selector found at \url{https://app.oceanbolt.com}.
#'
#' @param ladenStatus The laden status to get tonnage data for. The following
#' directions are allowed: \code{laden, ballast}.
#'
#' @param portStatus The port status to get tonnage data for. The following
#' directions are allowed: \code{in_port, at_sea}.
#'
#' @param excludeMpv Boolean, whether to exclude mpv vessels from the counts.
#'
#' @param token Oceanbolt API token, read from the environment by default
#'
#' @details
#' See \url{https://openapi.oceanbolt.com/#operation/getFleetSPeed} for details.
#'
#' *NOTE*: Parameter \code{format} is not used in this function and always set
#' to \code{"json"}.
#'
#' @return Data.table with average fleet speed for different
#' segments/zones/laden status.
#'
#' @examples
#' \dontrun{
#' speed <- getFleetSpeed()
#' speed
#' }
#'
#' @export
getFleetSpeed <- function(zoneId = 16,
                          segment = c(
                            "shortsea", "handysize", "supramax",
                            "panamax", "capesize"
                          ),
                          subSegment = c(),
                          direction = c(
                            "NNE", "ENE", "ESE", "SSE", "SSW",
                            "WSW", "WNW", "NNW"
                          ),
                          ladenStatus = c("laden", "ballast"),
                          portStatus = c("in_port", "at_sea"),
                          excludeMpv = TRUE,
                          token = Sys.getenv("OCEANBOLT_TOKEN")) {

  # Due to NSE notes in R CMD check / devtools::check()
  `segmentKey` <- `subSegmentKey` <- `:=` <- NULL

  # Checks options
  if (is.null(token)) {
    stop(paste0(
      "Please, register API token before using this function! ",
      "Use `registerToken(\"<YOUR_OCEANBOLT_TOKEN>\")`.\n",
      "See `?registerToken` for more details."
    ))
  }

  # Checks parameters validity
  # if ()
  # listZones()$zoneId



  # Checks parameters (transforms segments to sub-segments)
  if (length(segment) == 0) {
    segment <- c("shortsea", "handysize", "supramax", "panamax", "capesize")
  }
  .segment <- segment
  selectedSubSegments <- listSegments()[segmentKey %in% .segment]
  if (length(subSegment) > 0) {
    selectedSubSegments <- selectedSubSegments[subSegmentKey %in% subSegment]
  }
  selectedSubSegments <- selectedSubSegments$subSegmentKey

  # Queries API
  response <- POST(
    paste0(baseApiUrl, "/tonnage/speed"),
    add_headers(Authorization = paste0("Bearer ", token)),
    timeout(30),
    body = list(
      zoneId = zoneId,
      subSegment = selectedSubSegments,
      direction = direction,
      ladenStatus = ladenStatus,
      portStatus = portStatus,
      excludeMpv = excludeMpv,
      format = "json"
    ),
    encode = "json"
  )

  if (http_error(response)) {
    err <- content(response,
      as = "parsed", type = "application/json",
      encoding = "utf8"
    )
    stop(sprintf("Failed with error %d - %s", err$code, err$message))
  }

  parsed <- content(response,
    as = "text", type = "application/json",
    encoding = "utf8"
  )
  output <- setDT(fromJSON(parsed)[["fleetSpeedData"]])
  if (nrow(output) > 0) {
    output[, date := as.Date(date)]
  }

  output[]
}
