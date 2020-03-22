#' Create a new experiment
#'
#' Create a new experiment on Comet's servers. The return value is an [`Experiment`]
#' object that can be used to modify or get information about the experiment. Only one
#' experiment can be active at a time, so make sure to stop an experiment before creating
#' a new one (by calling the `stop()` method on the [`Experiment`] object).
#' @param experiment_name Experiment name.
#' @param project_name Project name (can also be specified using the `COMET_PROJECT_NAME`
#' parameter as an environment variable or in a comet config file).
#' @param workspace_name Workspace name (can also be specified using the `COMET_WORKSPACE`
#' parameter as an environment variable or in a comet config file).
#' @param api_key Comet API key (can also be specified using the `COMET_API_KEY`
#' parameter as an environment variable or in a comet config file).
#' @param log_stderr If `TRUE`, all output from 'stderr' (which includes errors,
#' warnings, and messages) will be redirected to the Comet servers to display as message
#' logs for the experiment. If `FALSE`, 'stderr' will be directed to its default handler
#' but its output will not be logged with the experiment.
#' @return An [`Experiment`] object.
#' @export
create_experiment <- function(
  experiment_name = NULL, project_name = NULL, workspace_name = NULL,
  api_key = NULL, log_stderr = FALSE
) {
  resp <- new_experiment(
    experiment_name = experiment_name,
    project_name = project_name,
    workspace_name = workspace_name,
    api_key = api_key
  )
  experiment_key <- resp[["experimentKey"]]
  experiment_link <- resp[["link"]]
  if (is.null(experiment_key) || is.null(experiment_link)) {
    comet_stop("Create experiment in Comet failed.")
  }
  LOG_INFO("Experiment created: ", experiment_link, echo = TRUE)

  LOG_DEBUG("Sending system details to the newly created experiment")
  try(
    set_system_details(experiment_key = experiment_key, api_key = api_key),
    silent = TRUE
  )

  .cometrenv$cancreate <- TRUE
  experiment <- Experiment$new(
    experiment_key = experiment_key,
    experiment_url = experiment_link,
    log_stderr = log_stderr,
    api_key = api_key
  )

  invisible(experiment)
}

#' @title A Comet Experiment object
#' @description
#' A comet experiment object can be used to modify or get information about an active
#' experiment. All methods documented here are the different ways to interact with an
#' experiment. Use [`create_experiment()`] to create a Comet experiment object.
#' @export
Experiment <- R6::R6Class(
  cloneable = FALSE,

  "Experiment",

  public = list(

    #' @description
    #' Do not call this function directly. Use `create_experiment()` instead.
    #' @param experiment_key N/A
    #' @param experiment_url N/A
    #' @param log_stderr N/A
    #' @param api_key N/A
    initialize = function(experiment_key, experiment_url = NULL,
                          log_stderr = FALSE, api_key = NULL) {
      if (!isTRUE(.cometrenv$cancreate)) {
        comet_stop("Do not call this function directly. Use `create_experiment()` instead.")
        return()
      }
      LOG_DEBUG("Creating experiment ", experiment_key)

      if (!is.null(.cometrenv$curexp)) {
        LOG_INFO("Existing experiment ", .cometrenv$curexp$get_experiment_key(), " will be stopped ",
                 "because a new experiment is active.", echo = TRUE)
        .cometrenv$curexp$stop()
      }

      .cometrenv$cancreate <- FALSE
      api_key <- api_key %||% get_config_api_key(must_work = TRUE)
      private$experiment_key <- experiment_key
      private$experiment_url <- experiment_url
      private$log_stderr <- log_stderr
      private$api_key <- api_key
      .cometrenv$curexp <- self
      private$keepalive_process <- create_keepalive_process(exp_key = experiment_key, api_key = api_key)

      private$logfile_path <- tempfile()
      private$logfile <- file(private$logfile_path, open = "w")
      private$log_offset_path <- paste0(private$logfile_path, ".offset")

      if (private$log_stderr) {
        sink(private$logfile, type = "message")
      }
      sink(private$logfile, type = "output", split = TRUE)
      private$logging_process <- create_logging_process(
        experiment_key = experiment_key, logfile_path = private$logfile_path,
        log_offset_path = private$log_offset_path, api_key = api_key
      )
    },

    #' @description
    #' Get the experiment key of an experiment.
    get_experiment_key = function() {
      private$experiment_key
    },

    #' @description
    #' Get the URL to view an experiment in the browser.
    get_experiment_url = function() {
      private$experiment_url
    },

    #' @description
    #' Get an experiment's system details.
    get_system_details = function() {
      get_system_details(experiment_key = private$experiment_key, api_key = private$api_key)
    },

    #' @description
    #' Get an experiment's HTML.
    get_html = function() {
      get_html(experiment_key = private$experiment_key, api_key = private$api_key)
    },

    #' @description
    #' Set (or append onto) an experiment's HTML.
    #' @param html (Required) An HTML string to add to the experiment.
    #' @param override If `TRUE`, override the previous HTML. If `FALSE`, append to it.
    set_html = function(html, override = NULL) {
      private$check_active()
      set_html(experiment_key = private$experiment_key, api_key = private$api_key,
               html = html, override = override)
      invisible(self)
    },

    #' @description
    #' Get an experiment's asset list.
    #' @param type The type of assets to retrieve (by default, all assets are returned).
    get_asset_list = function(type = NULL) {
      get_asset_list(experiment_key = private$experiment_key, api_key = private$api_key,
                     type = type)
    },

    #' @description
    #' Get an asset.
    #' @param assetId (Required) The asset ID to retrieve.
    get_asset = function(assetId) {
      get_asset(experiment_key = private$experiment_key, api_key = private$api_key,
                assetId = assetId)
    },

    #' @description
    #' Upload a file to the experiment.
    #' @param file (Required) Path to the file to upload.
    #' @param step Step number.
    #' @param overwrite If `TRUE`, overwrite any uploaded file with the same name.
    #' @param context The context.
    #' @param type The type of asset.
    #' @param name Name of the file on comet. By default the name of the file will
    #' match the file that you upload, but you can use this parameter to use a
    #' different name.
    #' @param metadata Metadata to upload along with the file.
    upload_asset = function(file, step = NULL, overwrite = NULL, context = NULL,
                           type = NULL, name = NULL, metadata = NULL) {
      private$check_active()
      upload_asset(experiment_key = private$experiment_key, api_key = private$api_key,
                   file = file, step = step, overwrite = overwrite,
                   context = context, type = type, name = name, metadata = metadata)
      invisible(self)
    },

    #' @description
    #' Stop an experiment. Always call this method before creating a new experiment.
    stop = function() {
      private$finalize()
      invisible(self)
    },

    #' @description
    #' Print the experiment.
    print = function() {
      cat("Comet experiment", private$experiment_key, "\n")
    }
  ),
  private = list(

    experiment_key = NULL,
    api_key = NULL,
    experiment_url = NULL,
    keepalive_process = NULL,

    log_stderr = NULL,
    logfile_path = NULL,
    logfile = NULL,
    log_offset_path = NULL,
    logging_process = NULL,

    check_active = function() {
      if (is.null(.cometrenv$curexp) ||
          self$get_experiment_key() != .cometrenv$curexp$get_experiment_key()) {
        comet_stop("This experiment already ended and cannot be modified.")
      }
    },

    finalize = function() {
      # If this is the active experiment, unset the active experiment
      if (!is.null(.cometrenv$curexp) && self$get_experiment_key() == .cometrenv$curexp$get_experiment_key()) {
        .cometrenv$curexp <- NULL
      }

      # Stop sending the keepalive signal
      if (!is.null(private$keepalive_process) && private$keepalive_process$is_alive()) {
        LOG_DEBUG("Stopping experiment ", private$experiment_key)
        private$keepalive_process$interrupt()
      }

      # Stop sending output logs
      if (!is.null(private$logging_process) && private$logging_process$is_alive()) {
        private$logging_process$interrupt()
      }

      # Stop redirecting output to log files
      suppressWarnings({
        sink(NULL, type = "output")
        if (private$log_stderr) {
          sink(NULL, type = "message")
        }
      })

      # Close output log file
      try(close(private$logfile), silent = TRUE)

      # Send the last output logs that haven't had a chance to be sent to Comet yet
      try({
        offset <- as.integer(readLines(private$log_offset_path))
        logfile <- file(private$logfile_path, open = "r")
        readLines(logfile, n = offset)
        new_messages <- readLines(logfile)
        close(logfile)
        if (length(new_messages) > 0) {
          log_output_lines(
            experiment_key = private$experiment_key,
            lines = new_messages,
            offset = offset,
            api_key = private$api_key
          )
        }
      }, silent = TRUE)

      # Remove the log files
      try(suppressWarnings(file.remove(private$logfile_path)), silent = TRUE)
      try(suppressWarnings(file.remove(private$log_offset_path)), silent = TRUE)
    }

  )
)

create_keepalive_process <- function(exp_key, api_key) {
  callr::r_bg(
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
    args = list(exp_key = exp_key, api_key = api_key)
  )
}

create_logging_process <- function(experiment_key, logfile_path, log_offset_path, api_key) {
  callr::r_bg(
    function(experiment_key, logfile_path, log_offset_path, api_key) {
      if (!file.exists(logfile_path)) {
        return()
      }
      cometr::disable_logging()
      logfile <- file(logfile_path, open = "r")
      offset <- 0
      writeLines(as.character(offset), log_offset_path)
      log_new_messages <- function() {
        new_messages <- readLines(logfile)
        if (length(new_messages) > 0) {
          asNamespace("cometr")$log_output_lines(
            experiment_key = experiment_key,
            lines = new_messages,
            offset = offset,
            api_key = api_key
          )
          offset <<- offset + length(new_messages)
          writeLines(as.character(offset), log_offset_path)
        }
      }

      while(TRUE) {
        log_new_messages()
        Sys.sleep(10)
      }
    },
    args = list(experiment_key = experiment_key, logfile_path = logfile_path,
                log_offset_path = log_offset_path, api_key = api_key)
  )
}
