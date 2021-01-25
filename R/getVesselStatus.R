#' Retrives live vessel status for any vessel or group of vessels in the dry
#' bulk fleet.
#'
#' @param direction The list of directions to get fleet speed data for.
#' The following directions are allowed: \code{NNE, ENE, ESE, SSE, SSW, WSW,
#' WNW, NNW}. Directions can also be obtained from the interactive direction
#' selector found at \url{https://app.oceanbolt.com}.
#'
#' @param ladenStatus The laden status to get tonnage data for. The following
#' directions are allowed: \code{laden, ballast}.
#'
#' @param imo The list of unique vessel identifiers (IMO numbers). This allows
#' to filter vessels data to show data only for a subset of vessels.
#'
#' @param loadCountry Sets load country filter for vessels. Full list of
#' countries can be obtained with \code{\link{listCountries}()}.
#'
#' @param destinationCountry Sets destination country filter for vessels. Full
#' list of countries can be obtained with \code{\link{listCountries}()}.
#'
#' @param loadRegion Sets load region filter for vessels. Full list of regions
#' can be obtained with \code{\link{listRegions}()}.
#'
#' @param destinationRegion Sets destination region filter for vessels. Full
#' list of regions can be obtained with \code{\link{listRegions}()}.
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
#' @param excludeUnknownDestinations Boolean, whether to exclude vessels with
#' unknown destinations.
#'
#' @param excludeMpv Boolean, whether to exclude mpv vessels from the counts.
#'
#' @param token Oceanbolt API token, read from the environment by default.
#'
#' @details
#' See \url{https://openapi.oceanbolt.com/#operation/vesselsLiveStatus} for
#' details.
#'
#' @return Data.table with vessels.
#'
#' @examples
#' \dontrun{
#' vesselStatus <- getVesselStatus()
#' vesselStatus
#' }
#'
#' @export
getVesselStatus <- function(direction = c("NNE", "ENE", "ESE", "SSE", "SSW",
                                          "WSW", "WNW", "NNW"),
                            ladenStatus = c("laden", "ballast"),
                            imo = c(),
                            loadCountry = c(),
                            destinationCountry = c(),
                            loadRegion = c(),
                            destinationRegion = c(),
                            zoneId = 16,
                            segment = c(
                              "shortsea", "handysize", "supramax",
                              "panamax", "capesize"
                            ),
                            subSegment = c(),
                            excludeUnknownDestinations = TRUE,
                            excludeMpv = FALSE,
                            token = Sys.getenv("OCEANBOLT_TOKEN")) {

  # Due to NSE notes in R CMD check / devtools::check()
  `segmentKey` <- `subSegmentKey` <- NULL

  # Checks options
  if (is.null(token)) {
    stop(paste0(
      "Please, register API token before using this function! ",
      "Use `registerToken(\"<YOUR_OCEANBOLT_TOKEN>\")`.\n",
      "See `?registerToken` for more details."
    ), call. = FALSE)
  }

  # Unifies parameters letter cases
  direction <- toupper(direction)
  ladenStatus <- tolower(ladenStatus)
  loadCountry <- toupper(loadCountry)
  destinationCountry <- toupper(destinationCountry)
  loadRegion <- toupper(loadRegion)
  destinationRegion <- toupper(destinationRegion)
  segment <- tolower(segment)
  subSegment <- tolower(subSegment)

  # Checks parameters validity
  if (!all(direction %in% c("NNE", "ENE", "ESE", "SSE",
                            "SSW", "WSW", "WNW", "NNW"))) {
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

  if (!all(loadCountry %in% unique(listCountries()$countryCode))) {
    stop(paste0(
      "Not a valid 'loadCountry'! ",
      "Please, check 'listCountries()' for valid values."
    ), call. = FALSE)
  }

  if (!all(destinationCountry %in% unique(listCountries()$countryCode))) {
    stop(paste0(
      "Not a valid 'destinationCountry'! ",
      "Please, check 'listCountries()' for valid values."
    ), call. = FALSE)
  }

  if (!all(loadRegion %in% unique(listRegions()$regionId))) {
    stop(paste0(
      "Not a valid 'loadRegion'! ",
      "Please, check 'listRegions()' for valid values."
    ), call. = FALSE)
  }

  if (!all(destinationRegion %in% unique(listRegions()$regionId))) {
    stop(paste0(
      "Not a valid 'destinationRegion'! ",
      "Please, check 'listRegions()' for valid values."
    ), call. = FALSE)
  }

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

  if (class(excludeUnknownDestinations) != "logical") {
    stop(paste0(
      "'excludeUnknownDestinations' should be of class 'logical'!"
    ), call. = FALSE)
  }

  if (class(excludeMpv) != "logical") {
    stop(paste0(
      "'excludeMpv' should be of class 'logical'!"
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
  response <- safeApiCall(
    POST,
    url = paste0(baseApiUrl, "/vessels/live"),
    add_headers(Authorization = paste0("Bearer ", token)),
    body = list(
      direction = direction,
      ladenStatus = ladenStatus,
      imo = imo,
      loadCountry = loadCountry,
      destinationCountry = destinationCountry,
      loadRegion = loadRegion,
      destinationRegion = destinationRegion,
      zoneId = zoneId,
      subSegment = selectedSubSegments,
      excludeUnknownDestinations = excludeUnknownDestinations,
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
    stop(sprintf(
      "Failed with HTTP code %d. Oceanbolt exit code %d - %s",
      status_code(response), err$code, err$message
    ), call. = FALSE)
  }

  parsed <- content(response,
                    as = "text", type = "application/json",
                    encoding = "utf8"
  )
  parsed <- fromJSON(parsed)
  if (length(parsed) == 0) {
    output <- data.table()
  } else {
    output <- setDT(parsed[["vessels"]])
  }
  output[]
}
