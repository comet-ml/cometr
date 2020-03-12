#' @export
experiment <- function(
  api_key = NULL, project_name = NULL, workspace = NULL, log_errors = FALSE
) {
  api_key <- get_config_api_key(api_key)
  if (is_config_empty(api_key)) {
    stop("You must provide an API key", call. = FALSE)
  }
  project_name <- get_config_project_name(project_name)
  workspace <- get_config_workspace(workspace)

  #TODO collect all startup info
  #TODO call update_status() continually
}

