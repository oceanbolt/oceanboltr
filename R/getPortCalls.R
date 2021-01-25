#' Retrives port calls for either a list of imos or list of ports.
#'
#' @param imo The list of unique vessel identifiers (IMO numbers). This allows
#' to filter tradeflows to show data only for a subset of vessels.
#'
#' @param portId List of portIds to get data for. Allowed values can be obtained
#' with \code{\link{listPorts}()}.
#'
#' @param unlocode List of UNLOCODES to get data for. Allowed values can be
#' obtained with \code{\link{listPorts}()}.
#'
#' @param page The page number to retrieve of the response. If \code{NULL} all
#' pages are returned.
#'
#' @param limit The number of voyages to retrieve per page (max is 1000,
#' default is 50) This parameter makes sense only if \code{page} in
#' no \code{NULL}.
#'
#' @param token Oceanbolt API token, read from the environment by default.
#'
#' @details
#' See \url{https://openapi.oceanbolt.com/#operation/vesselsPortCalls} for
#' details.
#'
#' @return Data.table with trade flows.
#'
#' @examples
#' \dontrun{
#' portCalls <- getPortCalls()
#' portCalls
#' }
#'
#' @export
getPortCalls <- function(imo = c(),
                         portId = listPorts()[region == "BALTIC"]$portId,
                         unlocode = c(),
                         page = NULL,
                         limit = 1000,
                         token = Sys.getenv("OCEANBOLT_TOKEN")) {

  # Due to NSE notes in R CMD check / devtools::check()
  `region` <- NULL

  # Checks options
  if (is.null(token)) {
    stop(paste0(
      "Please, register API token before using this function! ",
      "Use `registerToken(\"<YOUR_OCEANBOLT_TOKEN>\")`.\n",
      "See `?registerToken` for more details."
    ), call. = FALSE)
  }

  # Unifies parameters letter cases and parameters' type
  unlocode <- toupper(unlocode)

  if (!all(portId %in% unique(listPorts()$portId))) {
    stop(paste0(
      "Not a valid 'portId'! ",
      "Please, check 'listPorts()' for valid values."
    ), call. = FALSE)
  }

  if (!all(unlocode %in% unique(listPorts()$unlocode))) {
    stop(paste0(
      "Not a valid 'unlocode'! ",
      "Please, check 'listPorts()' for valid values."
    ), call. = FALSE)
  }

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
      url = paste0(baseApiUrl, "/portcalls"),
      add_headers(Authorization = paste0("Bearer ", token)),
      body = list(
        imo = imo,
        portId = portId,
        unlocode = unlocode,
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
