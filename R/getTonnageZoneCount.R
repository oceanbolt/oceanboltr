#' Retrieves tonnage data (vessel counts and dwt) for a given list of
#' zone_ids/segments/directions
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
#' speed <- getTonnageZoneCount()
#' speed
#' }
#'
#' @export
getTonnageZoneCount <- function(zoneId = 16,
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
    ), call. = FALSE)
  }

  # Unifies parameters letter cases
  segment <- tolower(segment)
  subSegment <- tolower(subSegment)
  direction <- toupper(direction)
  ladenStatus <- tolower(ladenStatus)
  portStatus <- tolower(portStatus)

  # Checks parameters validity
  if (!all(zoneId %in% listZones()$zoneId)) {
    stop(paste0(
      "Not a valid zoneId! ",
      "Please, check 'listZones()' for valid values."
    ), call. = FALSE)
  }

  if (!all(segment %in% unique(listSegments()$segmentKey))) {
    stop(paste0(
      "Not a valid segment! ",
      "Please, check 'listSegments()' for valid values."
    ), call. = FALSE)
  }

  if (!all(subSegment %in% unique(listSegments()$subSegmentKey))) {
    stop(paste0(
      "Not a valid sub-segment! ",
      "Please, check 'listSegments()' for valid values."
    ), call. = FALSE)
  }

  if (!all(direction %in% c(
    "NNE", "ENE", "ESE", "SSE",
    "SSW", "WSW", "WNW", "NNW"
  ))) {
    stop(paste0(
      "Not a valid direction! Should be in the list of values: ",
      "('NNE', 'ENE', 'ESE', 'SSE', 'SSW', 'WSW', 'WNW', 'NNW')"
    ), call. = FALSE)
  }

  if (!all(ladenStatus %in% c("laden", "ballast"))) {
    stop(paste0(
      "Not a valid laden status! Should be in the list of values: ",
      "('laden', 'ballast')"
    ), call. = FALSE)
  }

  if (!all(portStatus %in% c("in_port", "at_sea"))) {
    stop(paste0(
      "Not a valid port status! Should be in the list of values: ",
      "('in_port', 'at_sea')"
    ), call. = FALSE)
  }

  # Avoids conflict with columns' names from listSegment() output
  .segment <- segment
  .subSegment <- subSegment

  # Transforms segments to sub-segments
  if (length(.segment) == 0) {
    .segment <- c("shortsea", "handysize", "supramax", "panamax", "capesize")
  }

  selectedSubSegments <- listSegments()[segmentKey %in% .segment]
  if (length(.subSegment) > 0) {
    selectedSubSegments <- selectedSubSegments[subSegmentKey %in% .subSegment]
  }
  selectedSubSegments <- selectedSubSegments$subSegmentKey

  # Queries API
  response <- RETRY(
    "POST",
    url = paste0(baseApiUrl, "/tonnage/zone"),
    add_headers(Authorization = paste0("Bearer ", token)),
    body = list(
      zoneId = zoneId,
      subSegment = selectedSubSegments,
      direction = direction,
      ladenStatus = ladenStatus,
      portStatus = portStatus,
      excludeMpv = excludeMpv,
      format = "json"
    ),
    encode = "json",
    timeout(as.numeric(Sys.getenv("OCEANBOLT_RETRY_TIMEOUT", unset = 30))),
    times = as.numeric(Sys.getenv("OCEANBOLT_RETRY_TIMES", unset = 3)),
    pause_base = as.numeric(Sys.getenv("OCEANBOLT_RETRY_PAUSE_BASE", unset = 1)),
    pause_min = as.numeric(Sys.getenv("OCEANBOLT_RETRY_PAUSE_MIN", unset = 1)),
    pause_cap = as.numeric(Sys.getenv("OCEANBOLT_RETRY_PAUSE_CAP", unset = 60))
  )

  if (http_error(response)) {
    err <- content(response,
      as = "parsed", type = "application/json",
      encoding = "utf8"
    )
    stop(sprintf(
      "Failed with HTTP code %d. Oceanbolt exit code %d - %s",
      status_code(response), err$code, err$message
    ), call. = FALSE)
  }

  parsed <- content(response,
    as = "text", type = "application/json",
    encoding = "utf8"
  )
  output <- setDT(fromJSON(parsed)[["tonnageZoneCounts"]])
  if (nrow(output) > 0) {
    output[, date := as.Date(date)]
  }

  output[]
}
