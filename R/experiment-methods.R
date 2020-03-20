new_experiment <- function(
  experiment_name, project_name = NULL, workspace_name = NULL, api_key = NULL
) {
  project_name <- project_name %||% get_config_project_name(must_work = TRUE)
  workspace_name <- workspace_name %||% get_config_workspace(must_work = TRUE)

  endpoint <- "/write/experiment/create"
  method <- "POST"
  params <- list(
    experimentName = experiment_name,
    projectName = project_name,
    workspaceName = workspace_name
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

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
  if (missing(html) || is.null(html)) {
    comet_stop("`html` parameter must be provided.")
  }

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

upload_file <- function(experiment_key, file, step = NULL, overwrite = NULL,
                        context = NULL, type = NULL, name = NULL, metadata = NULL,
                        api_key = NULL) {
  if (missing(file) || is.null(file)) {
    comet_stop("`file` parameter must be provided.")
  }

  endpoint <- "/write/experiment/upload-asset"
  method <- "POST"
  params <- list(
    experimentKey = experiment_key,
    file = file,
    step = step,
    overwrite = overwrite,
    context = context,
    type = type,
    fileName = name,
    metadata = metadata
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}
