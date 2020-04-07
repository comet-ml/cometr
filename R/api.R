#' Get the Comet API version
#' @export
get_api_version <- function() {
  .cometrenv$COMET_API_VERSION
}

#' Call a Comet REST API endpoint
#'
#' This function is only meant for advanced users. If you would like to call any
#' arbitrary Comet API endpoint that isn't natively supported by `cometr`, you can
#' use this function.
#'
#' @inheritParams create_experiment
#' @param endpoint The REST API endpoint.
#' @param method The HTTP method to use, either "GET" or "POST".
#' @param params A list of parameters. For GET endpoints, the parameters are appended
#' to the URL; for POST endpoints, the parameters are sent in the body of the request.
#' @param response_json If `TRUE`, try to parse the response as JSON. If `FALSE`, return
#' the response as raw data (useful when the response is a file).
#' @return The parsed response
#' @export
call_api <- function(endpoint, method = c("GET", "POST"), params = list(), response_json = TRUE, api_key = NULL) {
  LOG_DEBUG("Call to API endpoint ", endpoint)
  method <- match.arg(method)
  api_key <- api_key %||% get_config_api_key(must_work = TRUE)

  auth <- httr::add_headers("Authorization" = api_key)
  agent <- httr::user_agent(sprintf("Comet API for R cometr/%s", utils::packageVersion(PACKAGE_NAME)))
  timeout <- httr::timeout(20)
  params <- Filter(Negate(is.null), params)
  url <- sprintf("%s%s%s", get_config_url(), .cometrenv$COMET_API_ENDPOINT_BASE, endpoint)

  tryCatch({
    if (endpoint == "/write/experiment/upload-asset" ||
        endpoint == "/write/experiment/git/patch") {
      LOG_INFO("API call: ", endpoint)
      body_params <- list(file = httr::upload_file(params$file))
      params$file <- NULL
      if (!is.null(params$metadata)) {
        body_params$metadata <- jsonlite::toJSON(params$metadata)
        params$metadata <- NULL
      }
      url <- httr::modify_url(url, query = params)
      response <- httr::POST(url, auth, agent, encode = "multipart", body = body_params, timeout)
    } else if (method == "GET") {
      url <- httr::modify_url(url, query = params)
      LOG_INFO("API call: ", method, " ", url)
      response <- httr::GET(url, auth, agent, timeout)
    } else if (method == "POST") {
      LOG_INFO("API call: ", method, " ", url, " ", params)
      response <- httr::POST(url, auth, agent, encode = "json", body = params, timeout)
    }
  }, error = function(err) {
    comet_stop("Error calling Comet API: ", err$message)
  })

  check_response(response)

  parsed <- parse_response(response, response_json = response_json)
  LOG_INFO("Parsed response: ", parsed)
  parsed
}

check_response <- function(res) {
  tryCatch({
    if (httr::status_code(res) != 200) {
      code <- httr::status_code(res)
      res <- try(parse_response(res), silent = TRUE)
      if (is.list(res) && !is.null(res[["msg"]])) {
        stop(res[["msg"]])
      } else {
        stop("Comet API response status was not OK (", code, ")")
      }
    }
  }, error = function(err) {
    comet_stop("Error with Comet API response status: ", err$message)
  })
  LOG_DEBUG("API response OK")
}

parse_response <- function(res, response_json = TRUE) {
  tryCatch({
    if (response_json) {
      if (httr::http_type(res) != "application/json") {
        stop("Comet API did not return json (", httr::http_type(res), ")")
      }
      parsed <- httr::content(res, as = "text", encoding = "UTF-8")
      parsed <- jsonlite::fromJSON(parsed, simplifyVector = FALSE)
      parsed
    } else {
      httr::content(res)
    }
  }, error = function(err) {
    comet_stop("Error parsing Comet API response: ", err$message)
  })
}
