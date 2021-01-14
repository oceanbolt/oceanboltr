#' Retrieves all the individual voyages for the given filter parameters.
#' Response is paginated, and endpoint accepts a paging parameter to specify
#' which page to return. It is also possible to set the number of voyages to
#' return per query, default is 50 and maximum is 2000.
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
#' @param page The page number to retrieve of the response. If \code{NULL} all
#' pages are returned.
#'
#' @param limit The number of voyages to retrieve per page (max is 2000, default
#' is 50). This parameter makes sense only if \code{page} in no \code{NULL}.
#'
#' @param token Oceanbolt API token, read from the environment by default.
#'
#' @details
#' See \url{https://openapi.oceanbolt.com/#operation/postTradeflowLadenLegs} for
#' details.
#'
#' @return Data.table with trade flows.
#'
#' @examples
#' \dontrun{
#' tradeFlows <- getTradeFlows()
#' tradeFlows
#' }
#'
#' @export
getTradeFlows <- function(commodity = c(
                            "iron_ore_fines", "iron_ore_pellets",
                            "iron_ore_unclassified", "magnetite"
                          ),
                          direction = c("export", "import"),
                          imo = c(),
                          segment = c(
                            "shortsea", "handysize", "supramax",
                            "panamax", "capesize"
                          ),
                          subSegment = c(),
                          fromDate = as.Date("2020-01-01"),
                          toDate = Sys.Date(),
                          fromCountryCode = c(),
                          toCountryCode = c(),
                          fromRegion = c(),
                          toRegion = c(),
                          excludeIntraCountry = TRUE,
                          excludeUnknownDestinations = FALSE,
                          page = NULL,
                          limit = 1000,
                          token = Sys.getenv("OCEANBOLT_TOKEN")) {

  # Due to NSE notes in R CMD check / devtools::check()
  `:=` <- `segmentKey` <- `subSegmentKey` <- `loadPortArrivedAt` <-
    `loadPortBerthedAt` <- `loadPortDepartedAt` <- `dischargePortArrivedAt` <-
    `dischargePortBerthedAt` <- `dischargePortDepartedAt` <- NULL

  # Checks options
  if (is.null(token)) {
    stop(paste0(
      "Please, register API token before using this function! ",
      "Use `registerToken(\"<YOUR_OCEANBOLT_TOKEN>\")`.\n",
      "See `?registerToken` for more details."
    ), call. = FALSE)
  }

  # Unifies parameters letter cases and parameters' type
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

  # Checks pagination settings
  if (limit > 2000) {
    limit <- 2000
    warning(paste0(
      "Maximum value of 'limit' is 2000, only 2000 results from the page ",
      "will be requested."
    ), call. = FALSE)
  }

  queryAllPages <- FALSE
  if (is.null(page)) {
    page <- 1
    queryAllPages <- TRUE
  }

  # Queries API
  output <- list()
  while (TRUE) {
    response <- safeApiCall(
      POST,
      url = paste0(baseApiUrl, "/tradeflows/data/voyages"),
      add_headers(Authorization = paste0("Bearer ", token)),
      body = list(
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
        excludeUnknownDestinations = excludeUnknownDestinations,
        page = page,
        limit = limit
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
    output[[length(output) + 1]] <- parsed[["data"]]

    if (!queryAllPages | page == parsed$maxPage) {
      break
    } else {
      page <- page + 1
    }
  }

  # Post-processing of results
  output <- setDT(rbindlist(output, use.names = TRUE, fill = TRUE))
  output[]
}
