.onLoad <- function(libname, pkgname) {
  # Load any config parameters that are in envvars or config files
  .cometenv$logging_enabled <- FALSE
  get_config_logging_file()
  get_config_logging_file_level()
  .cometenv$logging_enabled <- TRUE
  LOG_DEBUG("=== Starting new session of cometr ===")
  get_config_api_key()
  get_config_workspace()
  get_config_project_name()
  get_config_url()
}
