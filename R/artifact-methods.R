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
  artifact_id <- response$artifactId
  artifact_version_id <- response$artifactVersionId

  logged_artifact <- get_artifact(
    artifact_id = artifact_id,
    artifact_version_id = artifact_version_id,
    experiment_key = experiment_key,
    api_key = api_key
  )

  if (length(artifact$get_assets()) == 0) {
    LOG_WARNING("Artifact created without adding any assets, was this the intent?")
    update_artifact_version_state(
      artifact_version_id = artifact_version_id,
      state = "CLOSED",
      experiment_key = experiment_key,
      api_key = api_key
    )
    return(logged_artifact)
  }

  LOG_INFO(
    sprintf(
      "Artifact '%s/%s:%s' uploading started",
      logged_artifact$get_workspace(),
      logged_artifact$get_artifact_name(),
      as.character(logged_artifact$get_artifact_version())
    )
  )

  # log assets
  tryCatch({
    log_artifact_assets(
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

    LOG_INFO(
      sprintf(
        "Artifact '%s/%s:%s' has been successfully uploaded",
        logged_artifact$get_workspace(),
        logged_artifact$get_artifact_name(),
        as.character(logged_artifact$get_artifact_version())
      )
    )
  },
  error = function(err) {
    LOG_ERROR("Failed to log Artifact assets, reason: ", err)

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

  LOG_INFO(
    sprintf(
      "Scheduling the upload of %d assets for a size of %s, this can take some time.",
      num_assets,
      file_size_formated(total_size)
    )
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

    total_size <- total_size - asset$get_size()
    num_assets <- num_assets - 1

    if (num_assets > 0) {
      LOG_INFO(
        sprintf(
          "Still uploading %d artifact assets, remaining size %s",
          num_assets,
          file_size_formated(total_size)
        )
      )
    }
  }
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
  if (!is.null(artifact$artifact_version)){
    params$version = as.character(artifact$get_artifact_version())
  }
  if (!is.null(artifact$get_metadata())) {
    params$versionMetadata <- jsonlite::toJSON(artifact$get_metadata())
  }
  response <- call_api(
    endpoint = endpoint,
    method = method,
    params = params,
    api_key = api_key
  )

  current_version <- response$currentVersion
  previous_version <- response$previousVersion
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

  artifact_metadata <- result$metadata
  if (!is.null(artifact_metadata)) {
    artifact_metadata <-
      jsonlite::fromJSON(artifact_metadata, simplifyVector = FALSE)
  }
  LoggedArtifact$new(
    artifact_name = result$artifact$artifactName,
    artifact_type = result$artifact$artifactType,
    artifact_id = result$artifact$artifactId,
    artifact_version_id = result$artifactVersionId,
    workspace = result$artifact$workspaceName,
    experiment_key = experiment_key,
    artifact_version = result$artifactVersion,
    aliases = result$alias,
    artifact_tags = result$artifact$tags,
    version_tags = result$tags,
    size = result$sizeInBytes,
    metadata = artifact_metadata,
    source_experiment_key = result$experimentKey
  )
}
