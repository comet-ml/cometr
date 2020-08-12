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
#' @param keep_active If `TRUE`, automatically send Comet a status update every
#' few seconds until the experiment is stopped to mark the experiment as active on the
#' Comet web dashboard.
#' @param log_output If `TRUE`, all standard output will automatically be sent to
#' the Comet servers to display as message logs for the experiment. The output will still
#' be shown in the console as well.
#' @param log_error If `TRUE`, all output from 'stderr' (which includes errors,
#' warnings, and messages) will be redirected to the Comet servers to display as message
#' logs for the experiment. Note that unlike `auto_log_output`, if this option is on then
#' these messages will not be shown in the console and instead they will only be logged
#' to the Comet experiment. This option is set to `FALSE` by default because of this
#' behaviour.
#' @param log_code If `TRUE`, log the source code of the R script that was called
#' to Comet as the associated code of this experiment. This only works if the you run
#' a script using the `Rscript` tool and will not work in interactive sessions.
#' @param log_system_details If `TRUE`, automatically log the system details to
#' Comet when the experiment is created.
#' @param log_git_info If `TRUE`, log information about the active git repository.
#' Requires the `git2r` package to be installed.
#' @return An [`Experiment`] object.
#'
#' @examples
#' \dontrun{
#' library(cometr)
#' # Assuming you have COMET_API_KEY, COMET_WORKSPACE, COMET_PROJECT_NAME variables defined
#' exp <- create_experiment()
#' exp$get_key()
#' exp$get_metadata()
#' exp$add_tags(c("test", "tag2"))
#' exp$get_tags()
#' exp$log_metric("metric1", 5)
#' exp$get_metric("metric1")
#' exp$get_metrics_summary()
#' exp$stop()
#' }
#'
#' @export
create_experiment <- function(
  experiment_name = NULL, project_name = NULL, workspace_name = NULL, api_key = NULL,
  keep_active = TRUE, log_output = TRUE, log_error = FALSE,
  log_code = TRUE, log_system_details = TRUE, log_git_info = FALSE
) {
  base_experiment(
    experiment_name = experiment_name, project_name = project_name,
    workspace_name = workspace_name, api_key = api_key,
    keep_active = keep_active, log_output = log_output, log_error = log_error,
    log_code = log_code, log_system_details = log_system_details, log_git_info = log_git_info)
}

#' @title Get a previously created experiment
#' @description
#' Get a previously created experiment on Comet's servers. The return value is an [`Experiment`]
#' object that can be used to modify or get information about the experiment.
#' @param experiment_key Experiment key.
#' @param api_key Comet API key (can also be specified using the `COMET_API_KEY`
#' parameter as an environment variable or in a comet config file).
#' @param keep_active if `TRUE` keeps a communication channel open with comet.ml
#' @param log_output If `TRUE`, all standard output will automatically be sent to
#' the Comet servers to display as message logs for the experiment. The output will still
#' be shown in the console as well.
#' @param log_error If `TRUE`, all output from 'stderr' (which includes errors,
#' warnings, and messages) will be redirected to the Comet servers to display as message
#' logs for the experiment. Note that unlike `auto_log_output`, if this option is on then
#' these messages will not be shown in the console and instead they will only be logged
#' to the Comet experiment. This option is set to `FALSE` by default because of this
#' behaviour.
#' @param log_code If `TRUE`, log the source code of the R script that was called
#' to Comet as the associated code of this experiment. This only works if the you run
#' a script using the `Rscript` tool and will not work in interactive sessions.
#' @param log_system_details If `TRUE`, automatically log the system details to
#' Comet when the experiment is created.
#' @param log_git_info If `TRUE`, log information about the active git repository.
#' Requires the `git2r` package to be installed.
#' @return An [`Experiment`] object.
#'
#' @examples
#' \dontrun{
#' library(cometr)
#' # Assuming you have COMET_API_KEY, COMET_WORKSPACE, COMET_PROJECT_NAME variables defined
#' exp <- get_experiment("SOME-EXPERIMENT-KEY")
#' exp$get_key()
#' exp$get_metadata()
#' exp$add_tags(c("test", "tag2"))
#' exp$get_tags()
#' exp$log_metric("metric1", 5)
#' exp$get_metric("metric1")
#' exp$get_metrics_summary()
#' exp$stop()
#' }
#'
#' @export
get_experiment <- function(
  experiment_key, api_key = NULL, keep_active = FALSE, log_output = FALSE, log_error = FALSE,
  log_code = FALSE, log_system_details = FALSE, log_git_info = FALSE
) {
  base_experiment(api_key = api_key, keep_active = keep_active, log_output = log_output,
		  log_error = log_error, log_code = log_code, log_system_details = log_system_details,
		  log_git_info = log_git_info, experiment_key = experiment_key)
}

base_experiment <- function(
  experiment_name = NULL, project_name = NULL, workspace_name = NULL, api_key = NULL,
  keep_active = TRUE, log_output = TRUE, log_error = FALSE,
  log_code = TRUE, log_system_details = TRUE, log_git_info = FALSE,
  experiment_key = NULL
) {

  if (!isBool(keep_active)) {
    comet_stop("keep_active must be either TRUE or FALSE.")
  }
  if (!isBool(log_output)) {
    comet_stop("log_output must be either TRUE or FALSE.")
  }
  if (!isBool(log_error)) {
    comet_stop("log_error must be either TRUE or FALSE.")
  }
  if (!isBool(log_code)) {
    comet_stop("log_code must be either TRUE or FALSE.")
  }
  if (!isBool(log_system_details)) {
    comet_stop("log_system_details must be either TRUE or FALSE.")
  }
  if (!isBool(log_git_info)) {
    comet_stop("log_git_info must be either TRUE or FALSE.")
  }
  if (log_git_info && (!requireNamespace("git2r", quietly = TRUE) || utils::packageVersion("git2r") < "0.22.1")) {
    comet_stop("log_git_info requires you to have `git2r` version 0.22.1 or later.")
  }
  if (!is.null(experiment_key) &&
      (!is.null(workspace_name) || !is.null(project_name) || !is.null(experiment_name))) {
    comet_stop("If experiment_key is given, then workspace_name, project_name, and experiment_name must not be given.")
  }

  if (is.null(experiment_key)) {
    dynamic <- TRUE
    if (!is.null(.cometrenv$curexp)) {
      LOG_INFO("Existing experiment ", .cometrenv$curexp$get_key(), " will be stopped ",
               "because a new experiment is being created.", echo = TRUE)
      .cometrenv$curexp$stop()
    }

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
  } else {
    dynamic <- FALSE
    resp <- get_metadata(experiment_key)
    project_name <- resp[["projectName"]]
    workspace_name <- resp[["workspaceName"]]
    experiment_name <- resp[["experimentName"]]
    archived <- resp[["archived"]]
    experiment_link = create_experiment_link(base_url = modify_config_url(get_config_url()),
                                             workspace_name = workspace_name,
                                             project_name = project_name,
                                             experiment_key = experiment_key,
                                             archived = resp[["archived"]])
    LOG_INFO("Experiment retrieved: ", experiment_link, echo = TRUE)
  }

  if (log_code) {
    source_file <- get_system_script()
    if (!is.null(source_file) && file.exists(source_file)) {
      LOG_DEBUG("Logging source code to the newly created experiment from script ", source_file)
      try({
        source_code <- paste(readLines(source_file, warn = FALSE), collapse = "\n")
        log_code(experiment_key = experiment_key, code = source_code, api_key = api_key)
      }, silent = TRUE)
    }
  }

  if (log_system_details) {
    LOG_DEBUG("Logging system details to the newly created experiment")
    try({
      system_details <- get_all_system_details()
      log_system_details(experiment_key = experiment_key, details = system_details, api_key = api_key)
    }, silent = TRUE)
  }

  if (log_git_info) {
    LOG_DEBUG("Logging git information")
    git_details <- get_git_metadata_details()

    if (length(git_details) > 0) {
      try({
        LOG_DEBUG(git_details)
        log_git_metadata(experiment_key = experiment_key, details = git_details, api_key = api_key)

        patchfile <- get_git_patch_file()
        if (!is.null(patchfile) && file.exists(patchfile)) {
          log_git_patch(experiment_key = experiment_key, file = patchfile, api_key = api_key)
        }
      }, silent = TRUE)
    }
  }

  .cometrenv$cancreate <- TRUE
  experiment <- Experiment$new(
    experiment_key = experiment_key,
    experiment_url = experiment_link,
    api_key = api_key,
    keep_active = keep_active,
    log_output = log_output,
    log_error = log_error,
    dynamic = dynamic
  )

  invisible(experiment)
}

#' @title A Comet Experiment object
#' @description
#' A comet experiment object can be used to modify or get information about an active
#' experiment. All methods documented here are the different ways to interact with an
#' experiment. Use [`create_experiment()`] to create or [`get_experiment()`] to
#' retrieve a Comet experiment object.
#'
#' @examples
#' \dontrun{
#' library(cometr)
#' # Assuming you have COMET_API_KEY, COMET_WORKSPACE, COMET_PROJECT_NAME variables define
#' exp <- create_experiment()
#' exp$get_key()
#' exp$get_metadata()
#' exp$add_tags(c("test", "tag2"))
#' exp$get_tags()
#' exp$log_metric("metric1", 5)
#' exp$get_metric("metric1")
#' exp$get_metrics_summary()
#' exp$stop()
#' }
#'
#' @export
Experiment <- R6::R6Class(
  cloneable = FALSE,

  "Experiment",

  public = list(

    #' @description
    #' Do not call this function directly. Use `create_experiment()` or `get_experiment()` instead.
    initialize = function(experiment_key, experiment_url = NULL, api_key = NULL,
                          keep_active = FALSE, log_output = FALSE, log_error = FALSE,
                          dynamic = TRUE) {
      if (!isTRUE(.cometrenv$cancreate)) {
        comet_stop("Do not call this function directly. Use `create_experiment()` instead.")
      }
      .cometrenv$cancreate <- FALSE
      if (isTRUE(dynamic)) {
        LOG_DEBUG("Creating experiment ", experiment_key)
        .cometrenv$curexp <- self
      } else {
        LOG_DEBUG("Retrieving experiment ", experiment_key)
      }

      api_key <- api_key %||% get_config_api_key(must_work = TRUE)
      private$dynamic <- dynamic
      private$experiment_key <- experiment_key
      private$experiment_url <- experiment_url
      private$api_key <- api_key
      private$log_error <- log_error
      private$log_output <- log_output

      if (keep_active) {
        private$keepalive_process <- create_keepalive_process(exp_key = experiment_key, api_key = api_key)
        LOG_DEBUG("Created process ", private$keepalive_process$get_pid(), " to send keepalive signal.")
      }

      if (log_output || log_error) {
        private$logfile_path <- tempfile()
        private$logfile <- file(private$logfile_path, open = "w")
        private$log_offset_path <- paste0(private$logfile_path, ".offset")

        if (log_error) {
          suppressWarnings(
            sink(private$logfile, type = "message")
          )
        }
        if (log_output) {
          suppressWarnings(
            sink(private$logfile, type = "output", split = TRUE)
          )
        }

        private$logging_process <- create_logging_process(
          experiment_key = experiment_key, logfile_path = private$logfile_path,
          log_offset_path = private$log_offset_path, api_key = api_key
        )
        LOG_DEBUG("Created process ", private$logging_process$get_pid(), " to send output logs.")
      }

      if (isTRUE(dynamic)) {
        experiment_log_metadata(self)
      }
    },

    #' @description
    #' Get the experiment key of an experiment.
    get_key = function() {
      private$experiment_key
    },

    #' @description
    #' Get the dynamic status of an experiment.
    get_dynamic = function() {
      private$dynamic
    },

    #' @description
    #' Get the URL to view an experiment in the browser.
    get_url = function() {
      private$experiment_url
    },

    #' @description
    #' Get an experiment's metadata.
    get_metadata = function() {
      get_metadata(experiment_key = private$experiment_key, api_key = private$api_key)
    },

    #' @description
    #' Archive an experiment.
    archive = function() {
      private$check_active()
      archive_experiment(experiment_key = private$experiment_key, api_key = private$api_key)
      invisible(self)
    },

    #' @description
    #' Restore an archived experiment.
    restore = function() {
      private$check_active()
      restore_experiment(experiment_key = private$experiment_key, api_key = private$api_key)
      invisible(self)
    },

    #' @description
    #' Delete an experiment.
    delete = function() {
      private$finalize()
      delete_experiment(experiment_key = private$experiment_key, api_key = private$api_key)
    },

    #' @description
    #' Stop an experiment. Always call this method before creating a new experiment.
    stop = function() {
      private$finalize()
      invisible(self)
    },

    #' @description
    #' Log a metric name and value. Metrics are the only items that are logged as
    #' a full time series. However, even metrics can be throttled if too much data
    #' (either by rate or by count) is attempted to be logged.
    #' @param name (Required) Name of the metric.
    #' @param value (Required) Value of the metric.
    #' @param step Step number.
    #' @param epoch Epoch.
    #' @param context Context.
    log_metric = function(name, value, step = NULL, epoch = NULL, context = NULL) {
      private$check_active()
      log_metric(experiment_key = private$experiment_key, api_key = private$api_key,
                 name = name, value = value, step = step, epoch = epoch, context = context)
      invisible(self)
    },

    #' @description
    #' Get All Metrics For Name
    #' @param name (Required) Name of metric.
    get_metric = function(name) {
      get_metric(experiment_key = private$experiment_key, api_key = private$api_key,
                 name = name)
    },

    #' @description
    #' Get an experiment's metrics summary.
    get_metrics_summary = function() {
      get_metrics_summary(experiment_key = private$experiment_key, api_key = private$api_key)
    },

    #' @description
    #' Log an experiment's associated model graph.
    #' @param graph (Required) JSON representation of a graph.
    log_graph = function(graph) {
      private$check_active()
      log_graph(experiment_key = private$experiment_key, api_key = private$api_key,
                graph = graph)
      invisible(self)
    },

    #' @description
    #' Get an experiment's model graph.
    get_graph = function() {
      get_graph(experiment_key = private$experiment_key, api_key = private$api_key)
    },

    #' @description
    #' Log a parameter name and value. Note that you can only retrieve parameters
    #' summary data (e.g., this is not recorded as a full time series).
    #' @param name (Required) Name of the parameter.
    #' @param value (Required) Value of the parameter.
    #' @param step Step number.
    log_parameter = function(name, value, step = NULL) {
      private$check_active()
      log_parameter(experiment_key = private$experiment_key, api_key = private$api_key,
                    name = name, value = value, step = step)
      invisible(self)
    },

    #' @description
    #' Get an experiment's parameters summary.
    get_parameters = function() {
      get_parameters(experiment_key = private$experiment_key, api_key = private$api_key)
    },

    #' @description
    #' Log a key/value `other`` data (not a metric or parameter). Note
    #' that you can only retrieve others summary data (e.g., this is
    #' not recorded as a full time series).
    #' @param key (Required) The key.
    #' @param value (Required) The value.
    log_other = function(key, value) {
      private$check_active()
      log_other(experiment_key = private$experiment_key, api_key = private$api_key,
                key = key, value = value)
      invisible(self)
    },

    #' @description
    #' Get an experiment's others (logged with `log_other()`) summary.
    get_other = function() {
      get_other(experiment_key = private$experiment_key, api_key = private$api_key)
    },

    #' @description
    #' Add a list of tags to an experiment.
    #' @param tags (Required) List of tags.
    add_tags = function(tags) {
      private$check_active()
      add_tags(experiment_key = private$experiment_key, api_key = private$api_key,
               tags = as.list(tags))
      invisible(self)
    },

    #' @description
    #' Get an experiment's tags.
    get_tags = function() {
      get_tags(experiment_key = private$experiment_key, api_key = private$api_key)
    },

    #' @description
    #' Set (or append onto) an experiment's HTML.
    #' @param html (Required) An HTML string to add to the experiment.
    #' @param override If `TRUE`, override the previous HTML. If `FALSE`, append to it.
    log_html = function(html, override = FALSE) {
      private$check_active()
      log_html(experiment_key = private$experiment_key, api_key = private$api_key,
               html = html, override = override)
      invisible(self)
    },

    #' @description
    #' Get an experiment's HTML.
    get_html = function() {
      get_html(experiment_key = private$experiment_key, api_key = private$api_key)
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
    #' Add a symlink to an experiment in another project.
    #' @param project_name (Required) Project that the experiment to should linked to.
    create_symlink = function(project_name) {
      private$check_active()
      symlink_experiment(experiment_key = private$experiment_key, api_key = private$api_key,
                         project_name = project_name)
    },

    #' @description
    #' Log an experiment's git metadata. This should only be called once and it can be done
    #' automatically by enabling `log_git_info` in [`create_experiment()`] or [`get_experiment()`].
    #' This will replace any previous git metadata that was logged.
    #' @param branch Git branch name.
    #' @param origin Git repository origin.
    #' @param parent Git commit SHA.
    #' @param user Git username.
    #' @param root Git root.
    log_git_metadata = function(branch = NULL, origin = NULL, parent = NULL, user = NULL, root = NULL) {
      private$check_active()
      details <- list(
        branch = branch,
        origin = origin,
        parent = parent,
        user = user,
        root = root
      )
      log_git_metadata(experiment_key = private$experiment_key, details = details, api_key = private$api_key)
      invisible(self)
    },

    #' @description
    #' Get the git metadata of an experiment.
    get_git_metadata = function() {
      get_git_metadata(experiment_key = private$experiment_key, api_key = private$api_key)
    },

    #' @description
    #' Get the git patch of an experiment.
    get_git_patch = function() {
      patch <- get_git_patch(experiment_key = private$experiment_key, api_key = private$api_key)
      if (is.raw(patch)) {
        rawToChar(patch)
      } else {
        patch
      }
    },

    #' @description
    #' Get an experiment's standard output and error.
    get_output = function() {
      get_output(experiment_key = private$experiment_key, api_key = private$api_key)
    },

    #' @description
    #' Log an experiment's source code. This should only be called once and it can be done
    #' automatically by enabling `log_code` in [`create_experiment()`] or [`get_experiment()`].
    #' This will replace any previous code that was logged.
    #' @param code The code to set as the source code.
    log_code = function(code) {
      private$check_active()
      log_code(experiment_key = private$experiment_key, code = code, api_key = private$api_key)
      invisible(self)
    },

    #' @description
    #' Get an experiment's source code.
    get_code = function() {
      get_code(experiment_key = private$experiment_key, api_key = private$api_key)
    },

    #' @description
    #' Log system details. This can be done automatically by enabling `log_system_details`
    #' in [`create_experiment()`] or [`get_experiment()`].
    #' @param command Script and optional arguments.
    #' @param executable Executable.
    #' @param hostname Hostname.
    #' @param installed_packages List of installed R packages.
    #' @param gpu_static_info List of GPU information, where each GPU is a `list()` with
    #' fields `gpuIndex`, `name`, `powerLimit`, `totalMemory`, `uuid`.
    #' @param ip IP address.
    #' @param network_interface_ips List of network interface IPs.
    #' @param additional_system_info List of additional parameters to log,
    #' where each parameter is a `list()` with `key` and `value` pairs.
    #' @param os Full details about operating system.
    #' @param os_packages List of operating system packages installed.
    #' @param os_type Operating system type.
    #' @param pid Process ID.
    #' @param user User.
    #' @param r_version Short form R version.
    #' @param r_version_verbose Long form R version.
    log_system_details = function(
      command = NULL, executable = NULL, hostname = NULL, installed_packages = NULL, gpu_static_info = NULL,
      ip = NULL, network_interface_ips = NULL, additional_system_info = NULL, os = NULL,
      os_packages = NULL, os_type = NULL, pid = NULL, user = NULL, r_version = NULL,
      r_version_verbose = NULL
    ) {
      private$check_active()
      details <- list(
        command = as.list(command),
        executable = executable,
        hostname = hostname,
        installedPackages = as.list(installed_packages),
        gpuStaticInfoList = gpu_static_info,
        ip = ip,
        networkInterfaceIps = as.list(network_interface_ips),
        logAdditionalSystemInfoList = additional_system_info,
        os = os,
        osPackages = as.list(os_packages),
        osType = os_type,
        pid = pid,
        user = user,
        pythonVersion = r_version,
        pythonVersionVerbose = r_version_verbose
      )
      log_system_details(experiment_key = private$experiment_key, details = details, api_key = private$api_key)
      invisible(self)
    },

    #' @description
    #' Get an experiment's system details.
    get_system_details = function() {
      get_system_details(experiment_key = private$experiment_key, api_key = private$api_key)
    },

    #' @description
    #' Set an experiment's start and end time.
    #' @param start Start time for the experiment (milliseconds since the Epoch)
    #' @param end End time for the experiment (milliseconds since the Epoch)
    set_start_end_time = function(start = NULL, end = NULL) {
      private$check_active()
      if (is.null(start) && is.null(end)) {
        comet_stop("Either start or end must be provided.")
      }
      set_start_end_time(experiment_key = private$experiment_key, api_key = private$api_key,
                         start = start, end = end)
      invisible(self)
    },

    #' @description
    #' Print the experiment.
    print = function() {
      cat("Comet experiment", private$experiment_url, "\n")
    }
  ),

  private = list(

    experiment_key = NULL,
    api_key = NULL,
    experiment_url = NULL,
    dynamic = NULL,
    keepalive_process = NULL,

    log_output = NULL,
    log_error = NULL,
    logfile_path = NULL,
    logfile = NULL,
    log_offset_path = NULL,
    logging_process = NULL,

    check_active = function() {
      if (is.null(.cometrenv$curexp) ||
          self$get_key() != .cometrenv$curexp$get_key()) {
          comet_stop("This experiment already ended and cannot be modified.")
      }
    },

    finalize = function() {
      LOG_DEBUG("Experiment finalizer called on experiment ", self$get_key())
      suppressWarnings({
        # If this is the active experiment, unset the active experiment
        if (!is.null(.cometrenv$curexp) && self$get_key() == .cometrenv$curexp$get_key()) {
          LOG_DEBUG("Clearing cometr current experiment.")
          .cometrenv$curexp <- NULL
        }

        # Stop sending the keepalive signal
        if (!is.null(private$keepalive_process) && private$keepalive_process$is_alive()) {
          LOG_DEBUG("Stopping sendalive signal.")
          private$keepalive_process$interrupt()
        }

        # Stop logging output logs
        if (!is.null(private$logging_process) && private$logging_process$is_alive()) {
          LOG_DEBUG("Stopping output logging.")
          private$logging_process$interrupt()
        }

        # Stop redirecting output to log files
        if (private$log_output) {
          sink(NULL, type = "output")
        }
        if (private$log_error) {
          sink(NULL, type = "message")
        }

        if (private$log_output || private$log_error) {

          # Close output log file
          try(close(private$logfile), silent = TRUE)

          # Log the last output logs that haven't had a chance to be sent to Comet yet
          try({
            if (file.exists(private$log_offset_path)) {
              offset <- as.integer(readLines(private$log_offset_path))
            } else {
              offset <- 0
            }
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
          try(file.remove(private$logfile_path), silent = TRUE)
          try(file.remove(private$log_offset_path), silent = TRUE)
        }
      })
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
    args = list(exp_key = exp_key, api_key = api_key),
    supervise = TRUE
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
                log_offset_path = log_offset_path, api_key = api_key),
    supervise = TRUE
  )
}
