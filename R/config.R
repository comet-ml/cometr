COMET_CONFIG_FILE_NAME <- ".comet.yml"

get_config_api_key <- function(value = NULL) {
  get_config_param("COMET_API_KEY")
}

get_config_url <- function(value = NULL) {
  get_config_param("COMET_API_URL")
}

get_config_workspace <- function(value = NULL) {
  get_config_param("COMET_WORKSPACE")
}

get_config_project_name <- function(value = NULL) {
  get_config_param("COMET_PROJECT_NAME")
}

get_config_logging_file <- function(value = NULL) {
  get_config_param("COMET_LOGGING_FILE")
}

get_config_logging_file_level <- function(value = NULL) {
  get_config_param("COMET_LOGGING_FILE_LEVEL")
}

get_config_param <- function(name, default = NULL) {
  value <- get_config_from_value(value)
  if (!is_config_empty(value)) return(value)
  
  value <- get_config_from_envvar(varname)
  if (!is_config_empty(value)) return(value)
  
  value <- get_config_from_wd(varname)
  if (!is_config_empty(value)) return(value)
  
  value <- get_config_from_homedir(varname)
  if (!is_config_empty(value)) return(value)
  
  default
}

get_config_from_value <- function(value) {
  value
}

get_config_from_envvar <- function(name) {
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
    file <- file.path(dir, COMET_CONFIG_FILE_NAME)
    if (file.exists(file)) {
      configs <- yaml::read_yaml(file, eval.expr = TRUE)
      configs[[name]]
    } else {
      NULL
    }
  }, error = function(err) {
    NULL    
  })
}

is_config_empty <- function(value) {
  is.null(value) || value == ""
}