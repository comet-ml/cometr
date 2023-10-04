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
  response <-upsert_artifact(artifact = artifact,
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

  # log assets
  tryCatch({
    log_artifact_assets(
      artifact = artifact,
      artifact_version_id = artifact_version_id,
      logged_artifact_workspace = logged_artifact$get_workspace(),
      logged_artifact_name = logged_artifact$get_artifact_name(),
      logged_artifact_version = as.character(logged_artifact$get_artifact_version()),
      experiment_key = experiment_key,
      api_key = api_key
    )
    update_artifact_version_state(
      artifact_version_id = artifact_version_id,
      state = "CLOSED",
      experiment_key = experiment_key,
      api_key = api_key
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
                                logged_artifact_workspace,
                                logged_artifact_name,
                                logged_artifact_version,
                                experiment_key,
                                api_key = NULL) {
  assets <- artifact$get_assets()
  num_assets <- length(assets)
  total_size <-
    Reduce(function(u, v) u + v$get_size(), assets, right = FALSE)

  LOG_INFO(
    sprintf(
      "Scheduling the upload of %d assets for a size of %s, this can take some time.",
      num_assets,
      file_size_formated(total_size)
    )
  )

}

upsert_artifact <- function(artifact,
                            experiment_key,
                            api_key = NULL) {
  endpoint <- "write/artifacts/upsert"
  method <- "POST"
  params <- list(
    artifactName = artifact$get_artifact_name(),
    artifactType = artifact$get_artifact_type(),
    experimentKey = experiment_key,
    version = as.character(artifact$get_artifact_version()),
    alias = artifact$get_aliases(),
    versionTags = artifact$get_version_tags()
  )
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
  endpoint <- "write/artifacts/state"
  method <- "POST"
  params <- list(
    artifactVersionId = artifact_version_id,
    experimentKey = experiment_key,
    state = state
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
  endpoint <- "artifacts/version"
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
