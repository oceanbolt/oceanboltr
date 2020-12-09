#' Retrieves a list of all entities of a specific type available
#' in the trade flow database
#'
#' @description
#' This is a generic internal function to be used inside the package
#'
#' @param entity Type of the entity, one of \code{countries}, \code{regions},
#' \code{commodities}, \code{zones}, \code{segments}
#'
#' @keywords internal
#'
#' @return Data.table with available entities
listEntities <- function(entity = c("countries", "regions", "commodities",
                                    "zones", "segments")) {

  # Checks inputs
  if (missing(entity)) {
    stop(
      paste0("You should provide entity type of 'countries', 'regions',",
             "'commodities', 'zones' or 'segments'!")
    )
  }

  # If there are multiple options, then selects the first option
  if (length(entity) > 1) {
    entity <- entity[1]
  }

  if (!entity %in% c("countries", "regions", "commodities",
                     "zones", "segments")) {
    stop(
      paste0("Entity type should be one of: 'countries', 'regions',",
                "'commodities', 'zones' or 'segments'!")
    )
  }

  # Checks options
  baseUrl <- getOption("oceanbolt.base_url")
  token <- getOption("oceanbolt.token")

  if (is.null(baseUrl)) {
    stop(paste0("Please, reload package with `library(oceanboltr).`"))
  }

  if (is.null(token)) {
    stop(paste0(
      "Please, register API token before using this function! ",
      "Use `registerToken(\"<YOUR_OCEANBOLT_TOKEN>\")`.\n",
      "See `?registerToken` for more details."
    ))
  }

  # Queries API
  response <- GET(
    paste0(baseUrl, "/entities/", entity),
    add_headers(Authorization = paste0("Bearer ", token)),
    timeout(30)
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
  output <- setDT(fromJSON(parsed)[[entity]])

  return(output)
}
