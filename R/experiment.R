#' Create a new experiment
#' @param experiment_name Experiment name.
#' @param project_name Project name (can also be specified using the `COMET_PROJECT_NAME`
#' parameter as an environment variable or in a comet config file).
#' @param workspace_name Workspace name (can also be specified using the `COMET_WORKSPACE`
#' parameter as an environment variable or in a comet config file).
#' @param api_key Comet API key (can also be specified using the `COMET_API_KEY`
#' parameter as an environment variable or in a comet config file).
#' @param log_errors Whether or not to log errors.
#' @export
create_experiment <- function(
  experiment_name = NULL, project_name = NULL, workspace_name = NULL,
  api_key = NULL, log_errors = FALSE
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
  resp <- call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)

  experiment_key <- resp[["experimentKey"]]
  experiment_link <- resp[["link"]]
  if (is.null(experiment_key) || is.null(experiment_link)) {
    comet_stop("Create experiment in Comet failed.")
  }
  LOG_INFO("Experiment created: ", experiment_link)
  message("Experiment created: ", experiment_link)

  LOG_DEBUG("Sending system details to the newly created experiment")
  try(
    write_sysdetails(experiment_key = experiment_key, api_key = api_key),
    silent = TRUE
  )

  callr::r_bg(
    function(exp) {
      while(TRUE) {
        keepalive <- cometr::call_api("/write/experiment/set-status", "GET", list(experimentKey = exp))
        writeLines(as.character(Sys.time()), "ff.txt")
        sleeptime <- keepalive[["isAliveBeatDurationMillis"]]
        if (is.null(sleeptime)) {
          break
        }
        Sys.sleep(sleeptime / 1000)
      }
    },
    args = list(exp = experiment_key)
  )

  # keepalive_process$kill()

  #TODO set up stdout/stderr logging
  invisible(experiment_key)
}
