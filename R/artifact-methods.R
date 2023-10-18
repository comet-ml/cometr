get_artifact_files <- function(artifact_id = NULL,
                               workspace = NULL,
                               artifact_name = NULL,
                               version = NULL,
                               alias = NULL,
                               api_key = NULL) {
  endpoint <- "/artifacts/version/files"
  method <- "GET"
  params <- list(
    artifact_id = artifact_id,
    workspace = workspace,
    artifactName = artifact_name,
    version = version,
    alias = alias
  )
  call_api(
    endpoint = endpoint,
    method = method,
    params = params,
    api_key = api_key
  )
}

log_artifact <- function(artifact,
                         experiment_key,
                         api_key = NULL) {
  response <- upsert_artifact(artifact = artifact,
                              experiment_key = experiment_key,
                              api_key = api_key)
  artifact_id <- response[["artifactId"]]
  artifact_version_id <- response[["artifactVersionId"]]

  logged_artifact <- get_artifact(
    artifact_id = artifact_id,
    artifact_version_id = artifact_version_id,
    experiment_key = experiment_key,
    api_key = api_key
  )

  if (length(artifact$get_assets()) == 0) {
    LOG_WARNING("Artifact created without adding any assets, was this the intent?", echo = TRUE)
    update_artifact_version_state(
      artifact_version_id = artifact_version_id,
      state = "CLOSED",
      experiment_key = experiment_key,
      api_key = api_key
    )
    return(logged_artifact)
  }

  full_artifact_name <- create_full_artifact_name(
    artifact_name = logged_artifact$get_artifact_name(),
    workspace = logged_artifact$get_workspace(),
    version = as.character(logged_artifact$get_artifact_version())
  )

  LOG_INFO(
    sprintf(
      "Artifact '%s' uploading started",
      full_artifact_name
    ), echo = TRUE
  )

  # log assets
  tryCatch({
    total_size <- log_artifact_assets(
      artifact = artifact,
      artifact_version_id = artifact_version_id,
      experiment_key = experiment_key,
      api_key = api_key
    )
    update_artifact_version_state(
      artifact_version_id = artifact_version_id,
      state = "CLOSED",
      experiment_key = experiment_key,
      api_key = api_key
    )
    logged_artifact$size(total_size)

    LOG_INFO(
      sprintf(
        "Artifact '%s' has been successfully uploaded",
        full_artifact_name
      ), echo = TRUE
    )
  },
  error = function(err) {
    LOG_ERROR("Failed to log Artifact assets, reason: ", err, echo = TRUE)

    update_artifact_version_state(
      artifact_version_id = artifact_version_id,
      state = "ERROR",
      experiment_key = experiment_key,
      api_key = api_key
    )
  })
  logged_artifact
}

log_artifact_assets <- function(artifact,
                                artifact_version_id,
                                experiment_key,
                                api_key = NULL) {
  assets <- artifact$get_assets()
  num_assets <- length(assets)
  total_size <- 0
  for (asset in assets) {
    total_size <- total_size + asset$get_size()
  }

  remaining_bytes <- total_size

  LOG_INFO(
    sprintf(
      "Scheduling the upload of %d assets for a size of %s, this can take some time.",
      num_assets,
      file_size_formated(remaining_bytes)
    ), echo = TRUE
  )
  for (asset in assets) {
    if (asset$is_remote()) {
      upload_remote_asset(
        experiment_key = experiment_key,
        remote_uri = asset$get_link(),
        overwrite = asset$has_overwrite(),
        type = asset$get_asset_type(),
        name = asset$get_logical_path(),
        metadata = asset$get_metadata(),
        artifact_version_id = artifact_version_id,
        api_key = api_key
      )
    } else {
      upload_asset(
        experiment_key = experiment_key,
        file = asset$get_local_path(),
        overwrite = asset$has_overwrite(),
        type = asset$get_asset_type(),
        name = asset$get_logical_path(),
        metadata = asset$get_metadata(),
        artifact_version_id = artifact_version_id,
        api_key = api_key
      )
    }

    remaining_bytes <- remaining_bytes - asset$get_size()
    num_assets <- num_assets - 1

    if (num_assets > 0) {
      LOG_INFO(
        sprintf(
          "Still uploading %d artifact assets, remaining size %s",
          num_assets,
          file_size_formated(remaining_bytes)
        ), echo = TRUE
      )
    }
  }
  total_size
}

upsert_artifact <- function(artifact,
                            experiment_key,
                            api_key = NULL) {
  endpoint <- "/write/artifacts/upsert"
  method <- "POST"

  params <- list(
    artifactName = artifact$get_artifact_name(),
    artifactType = artifact$get_artifact_type(),
    experimentKey = experiment_key,
    alias = artifact$get_aliases(),
    versionTags = artifact$get_version_tags()
  )
  if (!is.null(artifact$get_artifact_version())){
    params$version = as.character(artifact$get_artifact_version())
  }
  if (!is.null(artifact$get_metadata())) {
    params$versionMetadata <- encode_metadata(artifact$get_metadata())
  }

  response <- call_api(
    endpoint = endpoint,
    method = method,
    params = params,
    api_key = api_key
  )

  current_version <- response[["currentVersion"]]
  previous_version <- response[["previousVersion"]]
  if (is.null(previous_version)) {
    LOG_INFO(
      sprintf(
        "Artifact '%s' version %s created",
        artifact$get_artifact_name(),
        current_version
      )
    )
  } else {
    LOG_INFO(
      sprintf(
        "Artifact '%s' version %s created (previous was: %s)",
        artifact$get_artifact_name(),
        current_version,
        previous_version
      )
    )
  }
  response
}

update_artifact_version_state <- function(artifact_version_id,
                                          state,
                                          experiment_key,
                                          api_key = NULL) {
  endpoint <- "/write/artifacts/state"
  method <- "POST"
  params <- list(
    artifactVersionId = artifact_version_id,
    experimentKey = experiment_key,
    state = state
  )
  response <- call_api(
    endpoint = endpoint,
    method = method,
    params = params,
    api_key = api_key,
    response_json = FALSE
  )
  response
}

update_arifact <- function(artifact_id,
                           artifact_type = NULL,
                           metadata = NULL,
                           version = NULL,
                           tags = NULL,
                           api_key = NULL) {
  if (!is.null(metadata)) {
    metadata <- encode_metadata(metadata)
  }
  endpoint <- "/write/artifacts/details"
  method <- "POST"
  params <- list(
    artifactId = artifact_id,
    artifactType = artifact_type,
    versionMetadata = metadata,
    version = version,
    tags = tags
  )
  call_api(
    endpoint = endpoint,
    method = method,
    params = params,
    api_key = api_key
  )
}

update_artifact_version <- function(artifact_version_id,
                                    version_aliases = NULL,
                                    version_metadata = NULL,
                                    version_tags = NULL,
                                    api_key = NULL) {
  if (!is.null(version_metadata)) {
    version_metadata <- encode_metadata(version_metadata)
  }
  endpoint <- "/write/artifacts/version/labels"
  method <- "POST"
  params <- list(
    artifactVersionId = artifact_version_id,
    alias = version_aliases,
    versionMetadata = version_metadata,
    versionTags = version_tags
  )
  call_api(
    endpoint = endpoint,
    method = method,
    params = params,
    api_key = api_key
  )
}

get_artifact <- function(workspace = NULL,
                         name = NULL,
                         artifact_id = NULL,
                         version = NULL,
                         alias = NULL,
                         artifact_version_id = NULL,
                         version_or_alias = NULL,
                         experiment_key = NULL,
                         consumer_experiment_key = NULL,
                         api_key = NULL) {
  endpoint <- "/artifacts/version"
  method <- "GET"
  params <- list(
    alias = alias,
    artifactId = artifact_id,
    artifactName = name,
    experimentKey = experiment_key,
    consumerExperimentKey = consumer_experiment_key,
    version = version,
    versionId = artifact_version_id,
    versionOrAlias = version_or_alias,
    workspace = workspace
  )
  result <- call_api(
    endpoint = endpoint,
    method = method,
    params = params,
    api_key = api_key
  )

  artifact_metadata <- result[["metadata"]]
  if (!is.null(artifact_metadata)) {
    artifact_metadata <- decode_metadata(artifact_metadata)
  }
  artifact <- result[["artifact"]]
  LoggedArtifact$new(
    artifact_name = artifact[["artifactName"]],
    artifact_type = artifact[["artifactType"]],
    artifact_id = artifact[["artifactId"]],
    artifact_version_id = result[["artifactVersionId"]],
    workspace = artifact[["workspaceName"]],
    experiment_key = experiment_key,
    artifact_version = result[["artifactVersion"]],
    aliases = result[["alias"]],
    artifact_tags = artifact[["tags"]],
    version_tags = result[["tags"]],
    size = result[["sizeInBytes"]],
    metadata = artifact_metadata,
    source_experiment_key = result[["experimentKey"]]
  )
}

get_artifact_by_name <- function(experiment_key,
                                 artifact_name,
                                 workspace = NULL,
                                 version_or_alias = NULL,
                                 api_key = NULL) {
  parsed <- parse_artifact_name(artifact_name = artifact_name)

  if (is.null(parsed$workspace) && is.null(workspace)) {
    # In that case, the backend will use the experiment id to get the workspace
    param_workspace <- NULL
  } else if (!is.null(parsed$workspace) && !is.null(workspace)) {
    LOG_WARNING(
      sprintf(
        "Workspace was given both explicitly '%s' and as part of the fully-qualified artifact name '%s', using the explicit value.",
        workspace,
        artifact_name
      )
    )
    param_workspace <- workspace
  } else if (is.null(workspace)) {
    param_workspace <- parsed$workspace
  } else {
    param_workspace <- workspace
  }

  if (!is.null(parsed$version_or_alias) &&
      !is.null(version_or_alias)) {
    LOG_WARNING(
      sprintf(
        "Version_or_alias was given both explicitly '%s' and as part of the fully-qualified artifact name '%s', using the explicit value.",
        version_or_alias,
        artifact_name
      )
    )
    param_version_or_alias <- version_or_alias
  } else if (!is.null(parsed$version_or_alias)) {
    param_version_or_alias <- parsed$version_or_alias
  } else {
    param_version_or_alias <- version_or_alias
  }

  get_artifact(workspace = param_workspace,
               name = parsed$artifact_name,
               version_or_alias = param_version_or_alias,
               experiment_key = experiment_key,
               consumer_experiment_key = experiment_key,
               api_key = api_key)
}

