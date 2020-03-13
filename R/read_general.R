#' Get a user's workspaces
#' @param api_key Comet API key.
#' @export
workspaces <- function(api_key = NULL) {
  endpoint <- "/workspaces"
  method <- "GET"
  params <- list()
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

#' Get a workspace's projects
#' @param workspace_name Workspace name.
#' @param api_key Comet API key.
#' @export
projects <- function(workspace_name = NULL, api_key = NULL) {
  workspace_name <- workspace_name %||% get_config_workspace(must_work = TRUE)

  endpoint <- "/projects"
  method <- "GET"
  params <- list(workspaceName = workspace_name)
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

#' Get a project's experiments
#'
#' Either `project_id` should be provided, or both `project_name` and `workspace_name`
#' should be provided. If `project_id` is provided, then `project_name` and `workspace_name`
#' are ignored.
#'
#' @param project_id Project ID.
#' @param project_name Project name.
#' @param workspace_name Workspace name.
#' @param archived Whether to retrieve archived experiments.
#' @param api_key Comet API key.
#' @export
experiments <- function(
  project_id = NULL, project_name = NULL, workspace_name = NULL, archived = FALSE, api_key = NULL
) {
  if (is.null(project_id)) {
    project_name <- project_name %||% get_config_project_name(must_work = TRUE)
    workspace_name <- workspace_name %||% get_config_workspace(must_work = TRUE)
  } else {
    project_name <- NULL
    workspace_name <- NULL
  }

  endpoint <- "/experiments"
  method <- "GET"
  params <- list(
    projectId = project_id,
    projectName = project_name,
    workspaceName = workspace_name,
    archived = archived
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}
