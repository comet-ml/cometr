# Get the value of a comet config parameter that can be set either as an envvar
# or config file. These functions have a side effect of storing the config value
# in a "cache" so that it can be retrieved in the future in the same session
# without having to do any I/O

get_config_api_key <- function(must_work = FALSE) {
  get_config_param("COMET_API_KEY", must_work = must_work)
}

get_config_workspace <- function(must_work = FALSE) {
  get_config_param("COMET_WORKSPACE", must_work = must_work)
}

get_config_project_name <- function(must_work = FALSE) {
  get_config_param("COMET_PROJECT_NAME", must_work = must_work)
}

get_config_url <- function() {
  get_config_param("COMET_URL_OVERRIDE", default = .cometrenv$COMET_API_DEFAULT_URL)
}

get_config_logging_file <- function() {
  get_config_param("COMET_LOGGING_FILE")
}

get_config_logging_file_level <- function() {
  get_config_param("COMET_LOGGING_FILE_LEVEL")
}

# Helper function to get the value of any comet config param, use a cache
# so that we don't constantly read config files
get_config_param <- function(param, default = NULL, must_work = FALSE) {
  if (is.null(.cometrenv$cache$config[[param]])) {
    value <- search_config_param(param = param, default = default)
    if (!is_config_empty(value)) {

      if (param == "COMET_LOGGING_FILE") {
        value <- modify_config_logging_file(value)
      }

      if (param == "COMET_URL_OVERRIDE") {
        value <- modify_config_url(value)
      }

      save_config_param(param = param, value = value)
    }
  }
  value <- .cometrenv$cache$config[[param]]
  if (must_work && is_config_empty(value)) {
    comet_stop(param, " must be provided")
  }
  value
}

# For log files - want to ensure we use the full path
# so that if the user changes directories during the analysis, same file is used
modify_config_logging_file <- function(value) {
  R.utils::getAbsolutePath(value, expandTilde = TRUE)
}

# For API base URL - strip the "/clientlib/" from the end
modify_config_url <- function(value) {
  sub("/clientlib[/]?$", "", value)
}

# Look for a config param in envvars and config files, or return a default if not found
search_config_param <- function(param, default = NULL) {
  value <- get_config_from_envvar(param)
  if (!is_config_empty(value)) return(value)

  value <- get_config_from_wd(param)
  if (!is_config_empty(value)) return(value)

  value <- get_config_from_homedir(param)
  if (!is_config_empty(value)) return(value)

  default
}

save_config_param <- function(param, value) {
  .cometrenv$cache$config[[param]] <- value
}

get_config_from_envvar <- function(name) {
  Sys.getenv(name, "")
}

get_config_from_wd <- function(name) {
  get_config_from_configfile(name, ".")
}

get_config_from_homedir <- function(name) {
  get_config_from_configfile(name, get_home_dir())
}

get_config_from_configfile <- function(name, dir) {
  tryCatch({
    file <- file.path(dir, get_config_filename())
    if (file.exists(file)) {
      configs <- suppressWarnings(yaml::read_yaml(file, eval.expr = TRUE))
      configs[[name]]
    } else {
      NULL
    }
  }, error = function(err) {
    warning("Error trying to read from config file: ", err$message)
    NULL
  })
}

get_config_filename <- function() {
  .cometrenv$COMET_CONFIG_FILE_NAME
}

get_home_dir <- function() {
  path.expand("~")
}

is_config_empty <- function(value) {
  is.null(value) || value == ""
}
