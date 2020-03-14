#' Create a new experiment
#' @param project_name Project name (can also be specified using the `COMET_PROJECT_NAME`
#' parameter as an environment variable or in a comet config file).
#' @param workspace_name Workspace name (can also be specified using the `COMET_WORKSPACE`
#' parameter as an environment variable or in a comet config file).
#' @param api_key Comet API key (can also be specified using the `COMET_API_KEY`
#' parameter as an environment variable or in a comet config file).
#' @param log_errors Whether or not to log errors.
#' @export
experiment <- function(
  project_name = NULL, workspace_name = NULL, api_key = NULL, log_errors = FALSE
) {
  #TODO collect all startup info
  #TODO call update_status() continually
  #TODO set up stdout/stderr logging
  #TODO set up internal logging
}

