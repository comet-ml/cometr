#' Get a user's workspaces
#' @inheritParams create_experiment
#'
#' @examples
#' \dontrun{
#' library(cometr)
#' # Assuming you have COMET_API_KEY variable defined
#' get_workspaces()
#' }
#'
#' @export
get_workspaces <- function(api_key = NULL) {
  endpoint <- "/workspaces"
  method <- "GET"
  params <- list()
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}

#' Get a workspace's projects
#' @inheritParams create_experiment
#'
#' @examples
#' \dontrun{
#' library(cometr)
#' # Assuming you have COMET_API_KEY, COMET_WORKSPACE variables defined
#' get_projects()
#' }
#'
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
#'
#' @examples
#' \dontrun{
#' library(cometr)
#' # Assuming you have COMET_API_KEY, COMET_WORKSPACE, COMET_PROJECT_NAME variables defined
#' get_experiments()
#' }
#'
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
#'
#' @examples
#' \dontrun{
#' library(cometr)
#' # Assuming you have COMET_API_KEY, COMET_WORKSPACE, COMET_PROJECT_NAME variables defined
#' get_columns()
#' }
#'
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
#'
#' @examples
#' \dontrun{
#' library(cometr)
#' # Assuming you have COMET_API_KEY, COMET_WORKSPACE variables defined
#' create_project(project_name = "project1", project_description = "My first project")
#' }
#'
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
#'
#' @examples
#' \dontrun{
#' library(cometr)
#' # Assuming you have COMET_API_KEY, COMET_WORKSPACE variables defined
#' delete_project(project_name = "project1")
#' }
#'
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

#' Get Multi-Metric Chart
#'
#' @inheritParams create_experiment
#' @param experiment_keys List of experiment keys.
#' @param metrics List of metric names to retrieve.
#' @param params List of parameter names to retrieve.
#' @param full Whether to fetch all values (up to 15,000) or a sampled subset (about 500 points).
#' @param independent Whether the metrics should be fetched individually or as a correlated whole
#' (only return values for steps for which you have values for every requested metric name).
#'
#' @examples
#' \dontrun{
#' library(cometr)
#' # Assuming you have COMET_API_KEY variable defined
#' experiment <- "<your experiment key>"
#' metrics <- c("<metric1>", "<metric2>")
#' get_multi_metric_chart(experiment_keys = experiment, metrics = metrics)
#' }
#'
#' @export
get_multi_metric_chart <- function(experiment_keys, metrics = list(), params = list(), full = TRUE,
                                   independent = TRUE, api_key = NULL) {
  endpoint <- "/experiments/multi-metric-chart"
  method <- "POST"
  params <- list(
    targetedExperiments = as.list(experiment_keys),
    metrics = as.list(metrics),
    params = as.list(params),
    fetchFull = full,
    independentMetrics = independent
  )
  call_api(endpoint = endpoint, method = method, params = params, api_key = api_key)
}
