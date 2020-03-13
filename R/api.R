#' Get the Comet API version
#' @export
get_api_version <- function() {
  .cometenv$COMET_API_VERSION
}

check_status <- function(res) {
  tryCatch({
    if (httr::status_code(res) != 200) {
      comet_stop("Comet API response status was not OK")
    }
    if (httr::http_type(res) != "application/json") {
      comet_stop("Comet API did not return json.")
    }
  }, error = function(err) {
    comet_stop("Error trying to check API response status: ", err$message)
  })
  LOG_DEBUG("API response OK")
}

call_api <- function(endpoint, method = c("GET", "POST"), params = list(), api_key = NULL) {
  LOG_DEBUG("Call to API endpoint ", endpoint)
  method <- match.arg(method)
  if (is.null(api_key)) {
    LOG_DEBUG("API key not explicitly provided")
    api_key <- get_config_api_key()
  }
  if (is_config_empty(api_key)) {
    comet_stop("API key not provided")
  }

  url <- sprintf("%s/%s%s", get_config_url(), .cometenv$COMET_API_ENDPOINT_BASE, endpoint)
  auth <- httr::add_headers("Authorization" = api_key)
  agent <- httr::user_agent(sprintf("Comet SDK for R cometr/%s", utils::packageVersion(PACKAGE_NAME)))

  LOG_INFO("API call: ", method, " ", url)

  if (method == "GET") {
    response <- httr::GET(url, auth, agent = agent)
  }
  check_status(response)

  parsed <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(parsed, simplifyVector = FALSE)
  parsed
}
