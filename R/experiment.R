#' @export
experiment <- function(
  api_key = NULL, project_name = NULL, workspace = NULL, log_errors = FALSE
) {
  api_key <- get_config_api_key(api_key)
  if (is_config_empty(api_key)) {
    stop("You must provide an API key.", call. = FALSE)
  }
  project_name <- get_config_project_name(project_name)
  workspace <- get_config_workspace(workspace)

  .cometenv$api_key <- api_key
  .cometenv$baseurl <- get_config_url()

  #TODO collect all startup info
  #TODO call update_status() continually
  #TODO set up stdout/stderr logging
  #TODO set up internal logging
}

