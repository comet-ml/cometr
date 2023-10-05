#' @import utils

`%||%` <- function(x, y) {
  if (length(x) > 0) x else y
}

epoch_ms <- function() {
  round(as.numeric(Sys.time()) * 1000)
}

isBool <- function(x) {
  isTRUE(x) || isFALSE(x)
}

get_values_from_list <- function(list, key) {
  unlist(lapply(list, function(element) element[[key]]))
}

generate_random_id <- function() {
  paste0(epoch_ms(), "-", paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ""))
}

experiment_log_metadata <- function(experiment) {
  experiment$log_other(key = "Created from", value = "cometr")
}

create_experiment_link <- function(base_url, workspace_name, project_name, experiment_key, archived) {
  if (archived) {
    experiment_link <- paste(
      base_url,
      workspace_name,
      "/",
      project_name,
      "/archived/",
      experiment_key,
      sep = ""
    )
  } else {
    experiment_link <- paste(
      base_url, workspace_name,
      "/",
      project_name,
      "/",
      experiment_key,
      sep = "")
  }
}

file_size_formated <- function(size){

  k = size/1024.0 ^ 1
  m = size/1024.0 ^ 2
  g = size/1024.0 ^ 3
  t = size/1024.0 ^ 4

  if (t > 1) {
    outSize = paste0(round(t,2),"TB")
  } else if (g > 1) {
    outSize = paste0(round(g,2),"GB")
  } else if (m > 1) {
    outSize = paste0(round(m,2),"MB")
  } else if (k > 1) {
    outSize = paste0(round(k,2),"KB")
  } else{
    outSize = paste0(round(size,2),"B")
  }

  return(outSize)
}

remote_asset_name_from_uri <- function(asset_uri) {
  # Try to parse the URI to see if we can extract a useful file name
  if (is.null(asset_uri) || is.na(asset_uri)) {
    logical_path <- "remote"
  } else {
    logical_path <- basename(asset_uri)
    if (logical_path == asset_uri) {
      splitted = strsplit(asset_uri, "/", fixed = TRUE)
      logical_path <- tail(splitted, n = 1)
    }
  }
  if (logical_path == "") {
    logical_path <- "remote"
  }
  logical_path
}
