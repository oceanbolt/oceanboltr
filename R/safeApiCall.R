#' Internal function to make safe API calls with retries on rate limits
#'
#' @param httrFunction Either \code{GET} or \code{POST} functions from
#'  \code{httr} package
#' @param ... Extra arguments for \code{httrFunction}
#'
#' @keywords internal
#'
#' @return Response object from \code{httrFunction}
safeApiCall <- function(httrFunction, ...) {

  # Check arguments
  if (missing(httrFunction)) {
    stop("You should specify 'httrFunction'!",
      call. = FALSE
    )
  }

  if (class(httrFunction) != "function") {
    stop("You should specify function from 'httr' package as 'httrFunction'!",
      call. = FALSE
    )
  }

  dots <- list(...)
  if (length(dots) < 1) {
    stop("You should provide arguments to 'httrFunction'!")
  }

  # Calling loop
  attempt <- 0
  nRetries <- as.numeric(Sys.getenv("OCEANBOLT_RETRY_TIMES", unset = 3))
  repeat {
    response <- httrFunction(
      ...,
      timeout(as.numeric(Sys.getenv("OCEANBOLT_RETRY_TIMEOUT", unset = 30)))
    )

    # Filter out everything but rate limiting
    if (status_code(response) != 429) {
      break
    } else {
      attempt <- attempt + 1
      if (attempt > nRetries) {
        break
      }

      # Check retry header
      retryAfter <- headers(response)["x-rate-limit-reset"]
      if (!is.null(retryAfter)) {
        retryAfter <- as.numeric(retryAfter)
      } else {
        retryAfter <- as.numeric(Sys.getenv("OCEANBOLT_RETRY_PAUSE", unset = 5))
        retryAfter <- retryAfter + as.numeric(Sys.time())
      }

      # Make a pause
      toSleep <- retryAfter - as.numeric(Sys.time())
      if (toSleep > 0) {
        message(sprintf(
          "Waiting for %0.2f seconds in order to respect API rate limiting...",
          toSleep
        ))
        Sys.sleep(toSleep)
      }
    }
  }

  response
}
