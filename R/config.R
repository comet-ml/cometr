# Get the value of a comet config parameter that can be set either as an envvar
# or config file. These functions have a side effect of storing the config value
# in a "cache" so that it can be retrieved in the future in the same session
# without having to do any I/O

get_config_api_key <- function() {
  get_config_param("COMET_API_KEY")
}

get_config_workspace <- function() {
  get_config_param("COMET_WORKSPACE")
}

get_config_project_name <- function() {
  get_config_param("COMET_PROJECT_NAME")
}

get_config_url <- function() {
  get_config_param("COMET_API_URL", default = .cometenv$COMET_API_DEFAULT_URL)
}

get_config_logging_file <- function() {
  .cometenv$logging_enabled <- FALSE
  on.exit(.cometenv$logging_enabled <- TRUE)
  get_config_param("COMET_LOGGING_FILE")
}

get_config_logging_file_level <- function() {
  .cometenv$logging_enabled <- FALSE
  on.exit(.cometenv$logging_enabled <- TRUE)
  get_config_param("COMET_LOGGING_FILE_LEVEL")
}

# Helper function to get the value of any comet config param, use a cache
# so that we don't constantly read config files
get_config_param <- function(param, default = NULL) {
  if (is.null(.cometenv$cache$config[[param]])) {
    LOG_DEBUG("Searching for config param `", param, "`")
    value <- search_config_param(param = param, default = default)
    save_config_param(param = param, value = value)
  }
  .cometenv$cache$config[[param]]
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
  LOG_DEBUG("Caching `", param, "` as `", value, "`")
  .cometenv$cache$config[[param]] <- value
}

get_config_from_envvar <- function(name) {
  LOG_DEBUG("Searching `", name, "` in envvars")
  Sys.getenv(name, "")
}

get_config_from_wd <- function(name) {
  get_config_from_configfile(name, ".")
}

get_config_from_homedir <- function(name) {
  get_config_from_configfile(name, path.expand("~"))
}

get_config_from_configfile <- function(name, dir) {
  tryCatch({
    file <- file.path(dir, .cometenv$COMET_CONFIG_FILE_NAME)
    LOG_DEBUG("Searching `", name, "` in config file ", file)
    if (file.exists(file)) {
      configs <- suppressWarnings(yaml::read_yaml(file, eval.expr = TRUE))
      configs[[name]]
    } else {
      LOG_DEBUG("Config file not found")
      NULL
    }
  }, error = function(err) {
    LOG_ERROR("Error trying to read from config file: ", err$message)
  })
}

is_config_empty <- function(value) {
  is.null(value) || value == ""
}
