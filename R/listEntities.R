#' Retrieves a list of all entities of a specific type available
#' in the trade flow database
#'
#' @description
#' This is a generic internal function to be used inside the package
#'
#' @param entity Type of the entity, one of \code{countries}, \code{regions},
#' \code{commodities}, \code{zones}, \code{segments}
#' @param token Oceanbolt API token
#'
#' @keywords internal
#'
#' @return Data.table with available entities
listEntities <- function(entity = c(
                           "countries", "regions", "commodities",
                           "zones", "segments", "ports"
                         ), token = Sys.getenv("OCEANBOLT_TOKEN")) {

  # Checks inputs
  if (missing(entity)) {
    stop(
      paste0(
        "You should provide entity type of 'countries', 'regions',",
        "'commodities', 'zones', 'segments' or 'ports'!"
      ),
      call. = FALSE
    )
  }

  # If there are multiple options, then selects the first option
  if (length(entity) > 1) {
    warning("Multiple entities are provided, using the first one...",
      call. = FALSE
    )
    entity <- entity[1]
  }

  if (!entity %in% c(
    "countries", "regions", "commodities",
    "zones", "segments", "ports"
  )) {
    stop(paste0(
      "You should provide entity type of 'countries', 'regions',",
      "'commodities', 'zones', 'segments' or 'ports'!"
    ), call. = FALSE)
  }

  # Checks options
  if (is.null(token)) {
    stop(paste0(
      "Please, register API token before using this function! ",
      "Use `registerToken(\"<YOUR_OCEANBOLT_TOKEN>\")`.\n",
      "See `?registerToken` for more details."
    ), call. = FALSE)
  }

  # Queries API
  response <- RETRY(
    "GET",
    url = paste0(baseApiUrl, "/entities/", entity),
    add_headers(Authorization = paste0("Bearer ", token)),
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
    ),
    call. = FALSE
    )
  }

  parsed <- content(response,
    as = "text", type = "application/json",
    encoding = "utf8"
  )
  output <- setDT(fromJSON(parsed)[[entity]])
  output
}
