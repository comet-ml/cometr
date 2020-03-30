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
  params <- list(experimentKey = experiment_key)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

get_system_details <- function(experiment_key, api_key = NULL) {
  endpoint <- "/experiment/system-details"
  method <- "GET"
  params <- list(experimentKey = experiment_key)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

log_system_details <- function(experiment_key, details = list(), api_key = NULL) {
  endpoint <- "/write/experiment/system-details"
  method <- "POST"
  params <- c(
    experimentKey = experiment_key,
    details
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

get_code <- function(experiment_key, api_key = NULL) {
  endpoint <- "/experiment/code"
  method <- "GET"
  params <- list(experimentKey = experiment_key)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

log_code <- function(experiment_key, code, api_key = NULL) {
  endpoint <- "/write/experiment/code"
  method <- "POST"
  params <- list(experimentKey = experiment_key, code = code)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

get_metadata <- function(experiment_key, api_key = NULL) {
  endpoint <- "/experiment/metadata"
  method <- "GET"
  params <- list(experimentKey = experiment_key)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

get_git_metadata <- function(experiment_key, api_key = NULL) {
  endpoint <- "/experiment/git/metadata"
  method <- "GET"
  params <- list(experimentKey = experiment_key)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

log_git_metadata <- function(experiment_key, details = list(), api_key = NULL) {
  endpoint <- "/write/experiment/git/metadata"
  method <- "POST"
  params <- c(
    experimentKey = experiment_key,
    details
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

get_git_patch <- function(experiment_key, api_key = NULL) {
  endpoint <- "/experiment/git/patch"
  method <- "GET"
  params <- list(experimentKey = experiment_key)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key, response_json = FALSE)
}

log_git_patch <- function(experiment_key, file, api_key = NULL) {
  endpoint <- "/write/experiment/git/patch"
  method <- "POST"
  params <- list(
    experimentKey = experiment_key,
    file = file
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

get_html <- function(experiment_key, api_key = NULL) {
  endpoint <- "/experiment/html"
  method <- "GET"
  params <- list(experimentKey = experiment_key)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

log_html <- function(experiment_key, html, override, api_key = NULL) {
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

get_asset_list <- function(experiment_key, type = NULL, api_key = NULL) {
  type <- type %||% "all"

  endpoint <- "/experiment/asset/list"
  method <- "GET"
  params <- list(
    experimentKey = experiment_key,
    type = type
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

get_asset <- function(experiment_key, assetId, api_key = NULL) {
  endpoint <- "/experiment/asset/get-asset"
  method <- "GET"
  params <- list(
    experimentKey = experiment_key,
    assetId = assetId
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key, response_json = FALSE)
}

upload_asset <- function(experiment_key, file, step = NULL, overwrite = NULL,
                         context = NULL, type = NULL, name = NULL, metadata = NULL,
                         api_key = NULL) {
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

get_output <- function(experiment_key, api_key = NULL) {
  endpoint <- "/experiment/output"
  method <- "GET"
  params <- list(experimentKey = experiment_key)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

# This is the function that should be called when `lines` has the correct format
# with each message having an offset
log_output <- function(experiment_key, lines, context = NULL, api_key = NULL) {
  endpoint <- "/write/experiment/output"
  method <- "POST"
  params <- list(
    experimentKey = experiment_key,
    outputLines = lines,
    context = context
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

# This is the function that should be called when an offset needs to be added
log_output_lines <- function(experiment_key, lines, offset = 0, context = NULL, api_key = NULL) {
  lines <- lapply(seq_along(lines), function(line_num) {
    list(offset = offset + line_num, output = lines[[line_num]])
  })
  log_output(experiment_key = experiment_key, lines = lines, context = context, api_key = api_key)
}

archive_experiment <- function(experiment_key, api_key = NULL) {
  endpoint <- "/write/experiment/archive"
  method <- "GET"
  params <- list(experimentKey = experiment_key)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key, response_json = FALSE)
}

restore_experiment <- function(experiment_key, api_key = NULL) {
  endpoint <- "/write/experiment/restore"
  method <- "GET"
  params <- list(experimentKey = experiment_key)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key, response_json = FALSE)
}

delete_experiment <- function(experiment_key, api_key = NULL) {
  endpoint <- "/write/experiment/delete"
  method <- "GET"
  params <- list(experimentKey = experiment_key)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key, response_json = FALSE)
}

symlink_experiment <- function(experiment_key, project_name, api_key = NULL) {
  endpoint <- "/write/project/symlink"
  method <- "GET"
  params <- list(experimentKey = experiment_key, projectName = project_name)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key, response_json = FALSE)
}

get_graph <- function(experiment_key, api_key = NULL) {
  endpoint <- "/experiment/graph"
  method <- "GET"
  params <- list(experimentKey = experiment_key)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

log_graph <- function(experiment_key, graph, api_key = NULL) {
  endpoint <- "/write/experiment/graph"
  method <- "POST"
  params <- list(experimentKey = experiment_key, graph = graph)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

get_tags <- function(experiment_key, api_key = NULL) {
  endpoint <- "/experiment/tags"
  method <- "GET"
  params <- list(experimentKey = experiment_key)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

add_tags <- function(experiment_key, tags, api_key = NULL) {
  endpoint <- "/write/experiment/tags"
  method <- "POST"
  params <- list(experimentKey = experiment_key, addedTags = tags)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

get_other <- function(experiment_key, api_key = NULL) {
  endpoint <- "/experiment/log-other"
  method <- "GET"
  params <- list(experimentKey = experiment_key)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

log_other <- function(experiment_key, key, value, api_key = NULL) {
  endpoint <- "/write/experiment/log-other"
  method <- "POST"
  params <- list(
    experimentKey = experiment_key,
    key = key,
    value = value,
    timestamp = epoch_ms()
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

get_metric <- function(experiment_key, name, api_key = NULL) {
  endpoint <- "/experiment/metrics/get-metric"
  method <- "GET"
  params <- list(experimentKey = experiment_key, metricName = name)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

get_metrics_summary <- function(experiment_key, api_key = NULL) {
  endpoint <- "/experiment/metrics/summary"
  method <- "GET"
  params <- list(experimentKey = experiment_key)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

log_metric <- function(
  experiment_key, name, value, step = NULL, epoch = NULL, context = NULL, api_key = NULL
) {
  endpoint <- "/write/experiment/metric"
  method <- "POST"
  params <- list(
    experimentKey = experiment_key,
    metricName = name,
    metricValue = value,
    step = step,
    epoch = epoch,
    context = context,
    timestamp = epoch_ms()
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

get_parameters <- function(experiment_key, api_key = NULL) {
  endpoint <- "/experiment/parameters"
  method <- "GET"
  params <- list(experimentKey = experiment_key)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

log_parameter <- function(experiment_key, name, value, step = NULL, api_key = NULL) {
  endpoint <- "/write/experiment/parameter"
  method <- "POST"
  params <- list(
    experimentKey = experiment_key,
    parameterName = name,
    parameterValue = value,
    step = step,
    timestamp = epoch_ms()
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

set_start_end_time <- function(experiment_key, start = NULL, end = NULL, api_key = NULL) {
  endpoint <- "/write/experiment/set-start-end-time"
  method <- "POST"
  params <- list(
    experimentKey = experiment_key,
    startTimeMillis = start,
    endTimeMillis = end
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}
