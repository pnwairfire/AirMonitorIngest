#'
#' @docType package
#' @name AirMonitorIngest
#' @title Data Ingest of Air Quality Monitoring Data
#' @description A suite of utility functions for ingesting air quality data
#' from various providers and converting that data into the compact data format
#' used by the \pkg{AirMonitor} package.
#'

NULL


# ----- Internal Package State -------------------------------------------------

AirMonitorIngestEnv <- new.env(parent = emptyenv())
AirMonitorIngestEnv$dataDir <- NULL
AirMonitorIngestEnv$apiKeys <- list(
  "airnow" = NULL,
  "custom1" = NULL,
  "custom2" = NULL
)

# ----- API Keys ---------------------------------------------------------------

#' @docType data
#' @keywords environment
#' @name apiKeys
#' @title API keys for data services.
#' @format List of character strings.
#' @description This package maintains an internal set of API keys which
#' users can set using \code{setAPIKey()}. These keys will be remembered for
#' the duration of an R session. The following service providers are supported:
#'
#' \itemize{
#' \item{\code{"airnow"}}
#' }
#'
#' @seealso \link{getAPIKey}
#' @seealso \link{setAPIKey}
NULL

#' @keywords environment
#' @export
#' @title Get API key
#' @param provider Web service provider.
#' @description Returns the API key associated with a web service.
#' If \code{provider == NULL} a list is returned containing all recognized
#' API keys.
#' @return API key string or a list of provider:key pairs.
#' @seealso \link{apiKeys}
#' @seealso \link{setAPIKey}

getAPIKey <- function(provider = NULL) {
  if ( is.null(provider) ) {
    return(AirMonitorIngestEnv$apiKeys)
  } else {
    if ( !(provider %in% names(AirMonitorIngestEnv$apiKeys)) ) {
      stop(sprintf(
        "Provider \"%s\" is not recognized.", provider
      ))
    } else {
      return(AirMonitorIngestEnv$apiKeys[[provider]])
    }
  }
}

#' @keywords environment
#' @export
#' @title Set APIKey
#' @param provider Web service provider.
#' @param key API key.
#' @description Sets the API key associated with a web service.
#' @return Silently returns previous value of the API key.
#' @seealso \link{LocationDataDir}
#' @seealso \link{getLocationDataDir}

setAPIKey <- function(provider = NULL, key = NULL) {
  if ( !(provider %in% names(AirMonitorIngestEnv$apiKeys)) ) {
    stop(sprintf(
      "Provider \"%s\" is not recognized.", provider
    ))
  } else {
    old <- AirMonitorIngestEnv$apiKeys[[provider]]
    AirMonitorIngestEnv$apiKeys[[provider]] <- key
    return(invisible(old))
  }
}
