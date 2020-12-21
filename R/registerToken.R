#' Registers Oceanbolt API token for further use
#'
#' @description
#' There are two types of access token registration: \code{plain} and
#' \code{keyring}. With \code{plain} you just pass token as-is and it is used
#' during your R session. \code{keyring} allows you to use your OS's keyring
#' backend (if enabled) which securely stores your token persistantly for all
#' further sessions.
#'
#' @param token Raw value of the Oceanbolt API token
#' @param type One of the two acceptable types of token registration
#'
#' @return Invisible NULL unless stops with error
#'
#' @examples
#' \dontrun{
#' # Register token as a plain text for the current session
#' registerToken("<YOUR_OCEANBOLT_TOKEN>")
#'
#' # Register token with your OS's keyring for further usage
#' registerToken("<YOUR_OCEANBOLT_TOKEN>", type = "keyring")
#' }
#'
#' @export
registerToken <- function(token, type = "plain") {

  # Checks inputs
  if (missing(token) | is.null(token) | is.na(token) | token == "") {
    stop("You should provide valid token!", call. = FALSE)
  }

  # If there are multiple options, then selects the first option
  if (length(type) > 1) {
    warning("Multiple token types are provided, first option will be used!",
      call. = FALSE
    )
    type <- type[1]
  }

  if (is.null(type) | is.na(type)) {
    stop("Token registration type should be one of 'plain' or 'keyring'!",
      call. = FALSE
    )
  }

  if (!type %in% c("plain", "keyring")) {
    stop("Token registration type should be one of 'plain' or 'keyring'.",
      call. = FALSE
    )
  }

  if (type == "keyring") {
    if (!requireNamespace("keyring", quietly = TRUE)) {
      stop(paste0(
        "In order to use keyring type of token registration, please, ",
        "install 'keyring' package with `install.packages(\"keyring\")`."
      ), call. = FALSE)
    } else {
      suppressWarnings(
        keyring::key_set_with_value("OCEANBOLT_TOKEN", password = token)
      )
    }
  }
  Sys.setenv("OCEANBOLT_TOKEN" = token)

  return(invisible(TRUE))
}
