#' Get a user's workspaces
#' @inheritParams create_experiment
#' @export
get_workspaces <- function(api_key = NULL) {
  endpoint <- "/workspaces"
  method <- "GET"
  params <- list()
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

#' Get a workspace's projects
#' @inheritParams create_experiment
#' @export
get_projects <- function(workspace_name = NULL, api_key = NULL) {
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
#' @inheritParams create_experiment
#' @param project_id Project ID.
#' @param archived If `TRUE`, retrieve archived experiments. Otherwise, retrieve
#' active experiments.
#' @export
get_experiments <- function(
  project_id = NULL, project_name = NULL, workspace_name = NULL, api_key = NULL, archived = FALSE
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

#' Get a project's columns
#'
#' Either `project_id` should be provided, or both `project_name` and `workspace_name`
#' should be provided. If `project_id` is provided, then `project_name` and `workspace_name`
#' are ignored.
#'
#' @inheritParams get_experiments
#' @export
get_columns <- function(
  project_id = NULL, project_name = NULL, workspace_name = NULL, api_key = NULL, archived = FALSE
) {
  if (is.null(project_id)) {
    project_name <- project_name %||% get_config_project_name(must_work = TRUE)
    workspace_name <- workspace_name %||% get_config_workspace(must_work = TRUE)
  } else {
    project_name <- NULL
    workspace_name <- NULL
  }

  endpoint <- "/project/column-names"
  method <- "GET"
  params <- list(
    projectId = project_id,
    projectName = project_name,
    workspaceName = workspace_name,
    archived = archived
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

#' Create a project
#'
#' @inheritParams get_experiments
#' @param project_name Project name.
#' @param project_description Project description.
#' @param public Whether the project should be public or private.
#' @export
create_project <- function(
  project_name, project_description, public = FALSE, workspace_name = NULL, api_key = NULL
) {
  workspace_name <- workspace_name %||% get_config_workspace(must_work = TRUE)

  endpoint <- "/write/project/create"
  method <- "POST"
  params <- list(
    isPublic = public,
    projectName = project_name,
    workspaceName = workspace_name,
    projectDescription = project_description
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

#' Delete a project
#'
#' @inheritParams get_experiments
#' @param project_name Project name.
#' @param delete_experiments If `TRUE`, delete all the experiments in the project.
#' @export
delete_project <- function(
  project_name, delete_experiments = TRUE, workspace_name = NULL, api_key = NULL
) {
  workspace_name <- workspace_name %||% get_config_workspace(must_work = TRUE)

  endpoint <- "/write/project/delete"
  method <- "POST"
  params <- list(
    deleteAllExperiments = delete_experiments,
    projectName = project_name,
    workspaceName = workspace_name
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

