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

parse_artifact_name <- function(artifact_name) {
  components <- strsplit(artifact_name, "/", fixed = TRUE)
  if (length(components[[1]]) == 1) {
    workspace <- NULL
    artifact_name_version <- components[[1]][[1]]
  } else {
    workspace <- components[[1]][[1]]
    artifact_name_version <- components[[1]][[2]]
  }

  name_version_components <- strsplit(artifact_name_version, ":", fixed = TRUE)
  if (length(name_version_components[[1]]) == 1) {
    artifact_name <- name_version_components[[1]][[1]]
    version_or_alias <- NULL
  } else {
    artifact_name <- name_version_components[[1]][[1]]
    version_or_alias <- name_version_components[[1]][[2]]
  }

  list(workspace=workspace, artifact_name=artifact_name, version_or_alias=version_or_alias)
}

create_full_artifact_name <- function(artifact_name, workspace, version) {
  name <- artifact_name
  if (!is.null(workspace)) {
    name <- paste0(workspace, "/", name)
  }
  if (!is.null(version)) {
    name <- paste0(name, ":", version)
  }
  name
}

encode_metadata <- function(metadata) {
  jsonlite::toJSON(metadata, auto_unbox = TRUE)
}

decode_metadata <- function(metadata) {
  jsonlite::fromJSON(metadata, simplifyVector = FALSE)
}

validate_artifact_overwrite_strategy <- function(overwrite_strategy) {
  if (isBool(overwrite_strategy)) {
    if (overwrite_strategy) {
      return("OVERWRITE")
    } else {
      return("FAIL")
    }
  } else if (is.character(overwrite_strategy)) {
    user_overwrite_strategy <- tolower(overwrite_strategy)
    if (user_overwrite_strategy == "fail") {
      return("FAIL")
    } else if (user_overwrite_strategy == "preserve") {
      return("PRESERVE")
    } else if (user_overwrite_strategy == "overwrite") {
      return("OVERWRITE")
    }
  }

    comet_stop("Unsupported overwrite_strategy value: ", overwrite_strategy)
  }

resolve_artifact_asset_path <- function(parent_dir,
                                        asset_file,
                                        overwrite_strategy) {
  asset_path <- file.path(parent_dir, asset_file)

  result <- list(asset_path=asset_path)
  result["already_exists"] <- FALSE
  if (file.exists(asset_path)) {
    result["already_exists"] <- TRUE
    if (overwrite_strategy == "OVERWRITE") {
      # remove existing file
      file.remove(asset_path)
    }
  } else {
    # create parent directories
    parent_dir <- dirname(asset_path)
    if (!dir.exists(parent_dir)) {
      dir.create(parent_dir, recursive = TRUE)
    }
  }
  result
}
