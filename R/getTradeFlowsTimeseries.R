#' Retrieves all summarized timeseries of exported volumes for the given filter
#' parameters. It is possible to supple a range parameter to determine
#' the interval of the timeseries returned (daily/weekly/monthly).
#'
#' @param range Determines the interval of the timeseries data. Allowed values
#' are: \code{("daily", "weekly", "monthly", "yearly")}.
#'
#' @param group Determines the grouping of the timeseries data. Allowed values
#' are: \code{"segment", "commodity", "commodity_group"}.
#'
#' @param commodity The commodity to get tradeflows for (get a list of all
#' commodities with \code{\link{listCommodities}()}).
#'
#' @param direction The flow direction of cargo. This parameter only applies for
#' top ports/top country queries. Allowed values are
#' \code{c("export", "import")}.
#'
#' @param imo The list of unique vessel identifiers (IMO numbers). This allows
#' to filter tradeflows to show data only for a subset of vessels.
#'
#' @param segment The list of vessel segments for which to get fleet speed data.
#' Allowed values can be obtained with \code{\link{listSegments}()}.
#'
#' @param subSegment The list of vessel sub segments for which to get fleet
#' speed data. Allowed values can be obtained with
#' \code{\link{listSegments}()}.
#'
#' @param fromDate The starting date to get tradeflows from.
#'
#' @param toDate The ending date to get tradeflows to.
#'
#' @param fromCountryCode The list of two letter ISO country codes for loading
#' (export) countries. Full list of countries can be get with
#' \code{\link{listCountries}()}.
#'
#' @param toCountryCode The list of two letter ISO country codes for discharge
#' (import) counties. Full list of countries can be get with
#' \code{\link{listCountries}()}.
#'
#' @param fromRegion The list of regions for loading (export) countries.
#'
#' @param toRegion The list of of regions for discharge (import) counties.
#'
#' @param excludeIntraCountry Determines whether or not to exclude voyages that
#' start and end in the same country (default to \code{TRUE}).
#'
#' @param excludeUnknownDestinations Determines whether or not to exclude
#' voyages that have unknown destinations (default to \code{FALSE}).
#'
#' @param token Oceanbolt API token, read from the environment by default.
#'
#' @details
#' See \url{https://openapi.oceanbolt.com/#operation/postTradeflowLadenLegs} for
#' details.
#'
#' @return Data.table with trade flows timeseries.
#'
#' @examples
#' \dontrun{
#' tradeFlowsTimeseries <- getTradeFlowsTimeseries()
#' tradeFlowsTimeseries
#' }
#'
#' @export
getTradeFlowsTimeseries <- function(
                                    range = c("daily", "weekly", "monthly", "yearly"),
                                    group = c("segment", "commodity", "commodity_group"),
                                    commodity = c(
                                      "iron_ore_fines", "iron_ore_pellets",
                                      "iron_ore_unclassified", "magnetite"
                                    ),
                                    direction = c("export", "import"),
                                    imo = c(),
                                    segment = c("shortsea", "handysize", "supramax", "panamax", "capesize"),
                                    subSegment = c(),
                                    fromDate = as.Date("2020-01-01"),
                                    toDate = Sys.Date(),
                                    fromCountryCode = c(),
                                    toCountryCode = c(),
                                    fromRegion = c(),
                                    toRegion = c(),
                                    excludeIntraCountry = TRUE,
                                    excludeUnknownDestinations = FALSE,
                                    token = Sys.getenv("OCEANBOLT_TOKEN")) {

  # Due to NSE notes in R CMD check / devtools::check()
  `:=` <- `segmentKey` <- `subSegmentKey` <- NULL

  # Checks options
  if (is.null(token)) {
    stop(paste0(
      "Please, register API token before using this function! ",
      "Use `registerToken(\"<YOUR_OCEANBOLT_TOKEN>\")`.\n",
      "See `?registerToken` for more details."
    ), call. = FALSE)
  }

  # Unifies parameters letter cases and parameters' type
  range <- tolower(range)
  group <- tolower(group)
  commodity <- tolower(commodity)
  direction <- tolower(direction)
  segment <- tolower(segment)
  subSegment <- tolower(subSegment)
  fromDate <- tryCatch(as.Date(fromDate), error = function(err) NULL)
  toDate <- tryCatch(as.Date(toDate), error = function(err) NULL)
  fromCountryCode <- toupper(fromCountryCode)
  toCountryCode <- toupper(toCountryCode)
  fromRegion <- toupper(fromRegion)
  toRegion <- toupper(toRegion)

  # Checks parameters validity
  if (length(range) >= 1) {
    if ("monthly" %in% range) {
      range <- "monthly"
    } else {
      range <- range[1]
    }

    if (!range %in% c("daily", "weekly", "monthly", "yearly")) {
      stop(paste0(
        "Not a valid range! Should be one of: ",
        "('daily', 'weekly', 'monthly', 'yearly')."
      ), call. = FALSE)
    }
  }

  if (length(group) >= 1) {
    group <- group[1]

    if (!group %in% c("segment", "commodity", "commodity_group")) {
      stop(paste0(
        "Not a valid group! Should be one of: ",
        "('segment', 'commodity', 'commodity_group')."
      ), call. = FALSE)
    }
  }

  if (!all(commodity %in% unique(listCommodities()$commodityValue))) {
    stop(paste0(
      "Not a valid commodity! ",
      "Please, check 'listCommodities()' for valid values."
    ), call. = FALSE)
  }

  if (length(direction) >= 1) {
    direction <- direction[1]

    if (!direction %in% c("export", "import")) {
      stop(paste0(
        "Not a valid direction! Should be one of: ('import', 'export')."
      ), call. = FALSE)
    }
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

  if (!"Date" %in% class(fromDate)) {
    stop(paste0(
      "'fromDate' should be of class 'Date'!"
    ), call. = FALSE)
  }

  if (!"Date" %in% class(toDate)) {
    stop(paste0(
      "'toDate' should be of class 'Date'!"
    ), call. = FALSE)
  }

  if (!all(fromCountryCode %in% unique(listCountries()$countryCode))) {
    stop(paste0(
      "Not a valid 'fromCountryCode'! ",
      "Please, check 'listCountries()' for valid values."
    ), call. = FALSE)
  }

  if (!all(toCountryCode %in% unique(listCountries()$countryCode))) {
    stop(paste0(
      "Not a valid 'toCountryCode'! ",
      "Please, check 'listCountries()' for valid values."
    ), call. = FALSE)
  }

  if (!all(fromRegion %in% unique(listRegions()$regionId))) {
    stop(paste0(
      "Not a valid 'fromRegion'! ",
      "Please, check 'listRegions()' for valid values."
    ), call. = FALSE)
  }

  if (!all(toRegion %in% unique(listRegions()$regionId))) {
    stop(paste0(
      "Not a valid 'toRegion'! ",
      "Please, check 'listRegions()' for valid values."
    ), call. = FALSE)
  }

  if (class(excludeIntraCountry) != "logical") {
    stop(paste0(
      "'excludeIntraCountry' should be of class 'logical'!"
    ), call. = FALSE)
  }

  if (class(excludeUnknownDestinations) != "logical") {
    stop(paste0(
      "'excludeUnknownDestinations' should be of class 'logical'!"
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
    url = paste0(baseApiUrl, "/tradeflows/data/timeseries"),
    add_headers(Authorization = paste0("Bearer ", token)),
    body = list(
      range = range,
      group = group,
      commodity = commodity,
      direction = direction,
      imo = imo,
      subSegment = selectedSubSegments,
      fromDate = fromDate,
      toDate = toDate,
      fromCountryCode = fromCountryCode,
      toCountryCode = toCountryCode,
      fromRegion = fromRegion,
      toRegion = toRegion,
      excludeIntraCountry = excludeIntraCountry,
      excludeUnknownDestinations = excludeUnknownDestinations
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
    output <- data.table(
      date = character(),
      value = integer(),
      group = character()
    )
  } else {
    output <- setDT(parsed[["data"]])
  }

  # Post-processing of results
  output
}
