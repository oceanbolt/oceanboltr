#' Retrieves live congestion data for vessels currently congestion.
#'
#' @param regionId The list of regionIds to get congestion data for. Allowed
#' values can be obtained with \code{\link{listRegions}()}.
#'
#' @param countryCode The list of countries to get congestion data for. Allowed
#' countries' codes can be obtained with \code{\link{listCountries}()}.
#'
#' @param commodity The list of commodities to get congestion data for. Allowed
#' values can be obtained with \code{\link{listCommodities}()}).
#'
#' @param commodityGroup The list of commodity groups to get congestion data
#' for. Allowed values can be obtained with \code{\link{listCommodities}()}).
#'
#' @param segment The list of vessel segments for which to get fleet speed data.
#' Allowed values can be obtained with \code{\link{listSegments}()}.
#'
#' @param subSegment The list of vessel sub segments for which to get fleet
#' speed data. Allowed values can be obtained with
#' \code{\link{listSegments}()}.
#'
#' @param lastLoadCountry The list of load countries to get congestion data for.
#' Allowed countries' codes can be obtained with \code{\link{listCountries}()}.
#'
#' @param operation The operational status of the vessel. By specifying this
#' filter it is possible to filter on the vessels that are waiting to discharge
#' ('discharge'), the that are waiting to load ('load), the vessels that are
#' waiting to go to yard ('yard). Allowed values are
#' \code{"discharge", "load", "yard", "unkown"}.
#'
#' @param token Oceanbolt API token, read from the environment by default.
#'
#' @details
#' See \url{https://openapi.oceanbolt.com/#operation/getCongestionLive} for
#' details.
#'
#' @return Data.table with data for vessels currently congestion.
#'
#' @examples
#' \dontrun{
#' liveCongestion <- getCongestionLive()
#' liveCongestion
#' }
#'
#' @export
getCongestionLive <- function(
  regionId = c("BALTIC"),
  countryCode = c(),
  commodity = c(
    "iron_ore_fines", "iron_ore_pellets",
    "iron_ore_unclassified", "magnetite"
  ),
  commodityGroup = c(),
  segment = c("shortsea", "handysize", "supramax", "panamax", "capesize"),
  subSegment = c(),
  lastLoadCountry = c(),
  operation = c("discharge", "load", "yard", "unkown"),
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

  # Unifies parameters letter cases and parameters' type
  regionId <- toupper(regionId)
  countryCode <- toupper(countryCode)
  commodity <- tolower(commodity)
  segment <- tolower(segment)
  subSegment <- tolower(subSegment)
  lastLoadCountry <- toupper(lastLoadCountry)
  operation <- tolower(operation)

  # Checks parameters validity
  if (!all(regionId %in% unique(listRegions()$regionId))) {
    stop(paste0(
      "Not a valid 'regionId'! ",
      "Please, check 'listRegions()' for valid values."
    ), call. = FALSE)
  }

  if (!all(countryCode %in% unique(listCountries()$countryCode))) {
    stop(paste0(
      "Not a valid 'countryCode'! ",
      "Please, check 'listCountries()' for valid values."
    ), call. = FALSE)
  }

  if (!all(commodity %in% unique(listCommodities()$commodityValue))) {
    stop(paste0(
      "Not a valid 'commodity'! ",
      "Please, check 'listCommodities()' for valid values."
    ), call. = FALSE)
  }

  if (!all(commodityGroup %in% unique(listCommodities()$commodityGroup))) {
    stop(paste0(
      "Not a valid 'commodityGroup'! ",
      "Please, check 'listCommodities()' for valid values."
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

  if (!all(lastLoadCountry %in% unique(listCountries()$countryCode))) {
    stop(paste0(
      "Not a valid 'lastLoadCountry'! ",
      "Please, check 'listCountries()' for valid values."
    ), call. = FALSE)
  }

  if (length(operation) >= 1) {
    operation <- operation[1]

    if (!all(operation %in% c("discharge", "load", "yard", "unkown"))) {
      stop(paste0(
        "Not a valid 'operation'! ",
        "Valid values are: 'discharge', 'load', 'yard', 'unkown'."
      ), call. = FALSE)
    }
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
    url = paste0(baseApiUrl, "/congestion/live"),
    add_headers(Authorization = paste0("Bearer ", token)),
    body = list(
      regionId = regionId,
      countryCode = countryCode,
      commodity = commodity,
      commodityGroup = commodityGroup,
      subSegment = selectedSubSegments,
      lastLoadCountry = lastLoadCountry,
      operation = operation
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
    output <- cbind(
      data.table(group = parsed[["timeseries"]]$group),
      parsed[["timeseries"]]$rows[[1]]
    )
  }

  # Post-processing of results
  output
}
