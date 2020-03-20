send_keepalive <- function(experiment_key, api_key = NULL) {
  endpoint <- "/write/experiment/set-status"
  method <- "GET"
  params <- list(
    experimentKey = experiment_key
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

get_html <- function(experiment_key, api_key = NULL) {
  endpoint <- "/experiment/html"
  method <- "GET"
  params <- list(experimentKey = experiment_key)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

set_html <- function(experiment_key, html, override = NULL, api_key = NULL) {
  endpoint <- "/write/experiment/html"
  method <- "POST"
  params <- list(
    experimentKey = experiment_key,
    html = html,
    override = override,
    timestamp = epoch_ms()
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}
