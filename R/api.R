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
#' @param parse_response If `TRUE`, try to parse response from server.
#' @param response_json If `TRUE`, try to parse the response as JSON. If `FALSE`, return
#' the response as raw data, e.g. binary.
#' @param local_file_path The path to the local file for saving downloaded content
#' if appropriate.
#' @return The parsed response
#' @export
call_api <- function(endpoint,
                     method = c("GET", "POST"),
                     params = list(),
                     parse_response = TRUE,
                     response_json = TRUE,
                     local_file_path = NULL,
                     api_key = NULL) {
  LOG_DEBUG("Call to API endpoint ", endpoint)
  method <- match.arg(method)
  api_key <- api_key %||% get_config_api_key(must_work = TRUE)

  auth <- httr::add_headers("Authorization" = api_key)
  agent <- httr::user_agent(sprintf("Comet API for R cometr/%s", utils::packageVersion(PACKAGE_NAME)))
  timeout <- httr::timeout(60)
  params <- Filter(Negate(is.null), params)
  url <- sprintf("%s%s%s", get_config_url(), .cometrenv$COMET_API_ENDPOINT_BASE, endpoint)

  tryCatch({
    if (endpoint == "/write/experiment/upload-asset" ||
        endpoint == "/write/experiment/git/patch") {
      body_params <- list()
      if (!is.null(params[["file"]])) {
        # local assets
        body_params$file <- httr::upload_file(params[["file"]])
        params[["file"]] <- NULL
      }
      if (!is.null(params$remote_uri)) {
        # remote assets
        body_params$link = params$remote_uri
        params[["remote_uri"]] <- NULL
      }

      if (!is.null(params$metadata)) {
        body_params$metadata <- encode_metadata(params$metadata)
        params[["metadata"]] <- NULL
      }

      url <- httr::modify_url(url, query = params)
      LOG_INFO("API call: ", method, " ", url, ", params: ", params, ", body_params ", body_params)
      response <- httr::POST(url, auth, agent, encode = "multipart", body = body_params, timeout)
    } else if (endpoint == "/experiment/asset/get-asset" && !is.null(local_file_path)) {
      # download asset to file
      parse_response <- FALSE
      url <- httr::modify_url(url, query = params)
      response <- httr::GET(url, auth, agent, timeout, httr::write_disk(local_file_path))
    } else if (method == "GET") {
      url <- httr::modify_url(url, query = params)
      LOG_INFO("API call: ", method, " ", url, ", params: ", params)
      response <- httr::GET(url, auth, agent, timeout)
    } else if (method == "POST") {
      LOG_INFO("API call: ", method, " ", url, ", params: ", params)
      response <- httr::POST(url, auth, agent, encode = "json", body = params, timeout)
    }
  }, error = function(err) {
    comet_stop("Error calling Comet API: ", err$message)
  })

  check_response(res = response, params = params)

  if (parse_response) {
    parsed <- parse_response(response, response_json = response_json)
    LOG_INFO("Parsed response: ", parsed)
    parsed
  } else {
    LOG_INFO("Response received: ", response)
    response
  }
}

check_response <- function(res, params = NULL) {
  tryCatch({
    if (httr::status_code(res) != 200) {
      code <- httr::status_code(res)
      res <- try(parse_response(res), silent = TRUE)
      if (is.list(res)) {
        if (!is.null(res[["sdk_error_code"]])) {
          sdk_error_code <- as.integer(res[["sdk_error_code"]])
          if (sdk_error_code == 624523) {
            comet_stop("Artifact not found with: ", params)
          } else if (sdk_error_code == 90403 || sdk_error_code == 90402) {
            comet_stop("Artifact is not in a finalized state and cannot be accessed with: ", params)
          }
        }
        if (!is.null(res[["msg"]])) {
          stop(res[["msg"]])
        }
      }
      stop("Comet API response status was not OK (", code, ")")
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
