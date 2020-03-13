#' Get the Comet API version
#' @export
get_api_version <- function() {
  .cometenv$COMET_API_VERSION
}

check_response <- function(res) {
  tryCatch({
    if (httr::status_code(res) != 200) {
      code <- httr::status_code(res)
      res <- parse_response(res)
      if (is.list(res) && !is.null(res[["msg"]])) {
        stop(res[["msg"]])
      } else {
        stop("Comet API response status was not OK (", code, ")")
      }
    }
    if (httr::http_type(res) != "application/json") {
      stop("Comet API did not return json (", httr::http_type(res), ")")
    }
  }, error = function(err) {
    comet_stop("Error with Comet API response status: ", err$message)
  })
  LOG_DEBUG("API response OK")
}

call_api <- function(endpoint, method = c("GET", "POST"), params = list(), api_key = NULL) {
  LOG_DEBUG("Call to API endpoint ", endpoint)
  method <- match.arg(method)
  api_key <- api_key %||% get_config_api_key(must_work = TRUE)

  auth <- httr::add_headers("Authorization" = api_key)
  agent <- httr::user_agent(sprintf("Comet SDK for R cometr/%s", utils::packageVersion(PACKAGE_NAME)))
  url <- sprintf("%s%s%s", get_config_url(), .cometenv$COMET_API_ENDPOINT_BASE, endpoint)
  url <- httr::modify_url(url, query = params)

  LOG_INFO("API call: ", method, " ", url)

  if (method == "GET") {
    response <- httr::GET(url, auth, agent)
  }
  check_response(response)
  LOG_DEBUG("Request: ", response$request)

  parsed <- parse_response(response)
  LOG_INFO("Parsed response: ", parsed)
  parsed
}

parse_response <- function(res) {
  parsed <- httr::content(res, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(parsed, simplifyVector = FALSE)
  parsed
}
