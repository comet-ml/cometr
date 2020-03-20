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

  if (!is.null(.cometrenv$curexp)) {
    LOG_INFO("Existing experiment ", .cometrenv$curexp$get_experiment_key(), " will be stopped ",
             "because a new experiment is being created.", echo = TRUE)
    .cometrenv$curexp$stop()
  }

  project_name <- project_name %||% get_config_project_name(must_work = TRUE)
  workspace_name <- workspace_name %||% get_config_workspace(must_work = TRUE)
  api_key <- api_key %||% get_config_api_key(must_work = TRUE)
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
  LOG_INFO("Experiment created: ", experiment_link, echo = TRUE)

  LOG_DEBUG("Sending system details to the newly created experiment")
  try(
    write_sysdetails(experiment_key = experiment_key, api_key = api_key),
    silent = TRUE
  )

  .cometrenv$cancreate <- TRUE
  experiment <- Experiment$new(experiment_key = experiment_key, api_key = api_key)
  .cometrenv$curexp <- experiment

  #TODO set up stdout/stderr logging
  invisible(experiment)
}

#' @title vv
#' @description pp
Experiment <- R6::R6Class(
  cloneable = FALSE,

  "Experiment",

  public = list(

    #' @description
    #' Do not call this function directly. Use `create_experiment()` instead.
    #' @param experiment_key Experiment key.
    #' @param api_key API key.
    initialize = function(experiment_key, api_key) {
      if (!isTRUE(.cometrenv$cancreate)) {
        comet_stop("Do not call this function directly. Use `create_experiment()` instead.")
        return()
      }
      LOG_DEBUG("Creating experiment ", experiment_key)
      .cometrenv$cancreate <- FALSE
      private$experiment_key <- experiment_key
      private$api_key <- api_key

      private$keepalive_process <- callr::r_bg(
        function(exp_key, api_key) {
          cometr::disable_logging()
          while(TRUE) {
            keepalive <- asNamespace("cometr")$send_keepalive(experiment_key = exp_key, api_key = api_key)
            sleeptime <- keepalive[["isAliveBeatDurationMillis"]]
            if (is.null(sleeptime)) {
             break
            }
            Sys.sleep(sleeptime / 1000)
          }
        },
        args = list(exp_key = private$experiment_key, api_key = private$api_key)
      )
    },

    #' @description
    #' Print the experiment.
    print = function() {
      cat("Comet experiment", private$experiment_key, "\n")
    },

    #' @description
    #' Stop an experiment.
    stop = function() {
      private$finalize()
      invisible(self)
    },

    #' @description
    #' Get the experiment key of an experiment.
    #' @export
    get_experiment_key = function() {
      private$experiment_key
    },

    #' @description
    #' Get an experiment's HTML.
    get_html = function() {
      get_html(experiment_key = private$experiment_key, api_key = private$api_key)
    },

    #' @description
    #' Set (or append onto) an experiment's HTML.
    #' @param html An HTML string to add to the experiment.
    #' @param override If `TRUE`, override the previous HTML. If `FALSE`, append to it.
    set_html = function(html, override = NULL) {
      private$check_active()
      set_html(experiment_key = private$experiment_key, api_key = private$api_key,
               html = html, override = override)
      invisible(self)
    }

  ),
  private = list(

    experiment_key = NULL,
    api_key = NULL,
    keepalive_process = NULL,

    check_active = function() {
      if (self$get_experiment_key() != .cometrenv$curexp$get_experiment_key()) {
        comet_stop("This experiment already ended and cannot be modified.")
      }
    },

    finalize = function() {
      LOG_DEBUG("Stopping experiment ", private$experiment_key)
      if (!is.null(.cometrenv$curexp) && self$get_experiment_key() == .cometrenv$curexp$get_experiment_key()) {
        .cometrenv$curexp <- NULL
      }
      if (!is.null(private$keepalive_process) && private$keepalive_process$is_alive()) {
        private$keepalive_process$kill()
      }
    }

  )
)
