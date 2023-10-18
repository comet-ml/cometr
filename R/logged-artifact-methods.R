download_artifact_asset <- function(experiment_key,
                                    asset_id,
                                    artifact_version_id,
                                    asset_filename,
                                    root_path,
                                    overwrite_strategy,
                                    asset_type,
                                    metadata,
                                    api_key = NULL) {

  overwrite_strategy <- validate_artifact_overwrite_strategy(overwrite_strategy)

  resolved <- resolve_artifact_asset_path(
    parent_dir = root_path,
    asset_file = asset_filename,
    overwrite_strategy = overwrite_strategy
  )

  if (resolved$already_exists && overwrite_strategy == "PRESERVE") {
    # preventing original file - just warning and return ArtifactAsset pointing to it
    LOG_INFO(sprintf("Preserving original Artefact asset '%s' at file '%s' due to selected overwrite strategy.",
                     asset_filename, resolved$asset_path), echo = TRUE)

    asset <- create_asset_from_file(
      asset_file = resolved$asset_path,
      logical_path = asset_filename,
      overwrite = FALSE,
      metadata = metadata,
      asset_type = asset_type
    )
    return(asset)
  } else if (resolved$already_exists && overwrite_strategy == "FAIL") {
    # create temporary file name to download
    local_file_path <- tempfile()
  } else {
    # regular download or OVERWRITE (target file already removed by resolve_artifact_asset_path())
    local_file_path <- resolved$asset_path
  }
  asset_file_path <- resolved$asset_path

  LOG_INFO(sprintf("Downloading Artefact asset '%s' to file '%s'",
                   asset_filename, local_file_path), echo = TRUE)

  method <- "GET"
  endpoint <- "/experiment/asset/get-asset"
  params <- list(
    experimentKey = experiment_key,
    assetId = asset_id,
    artifactVersionId = artifact_version_id
  )
  call_api(
    endpoint = endpoint,
    method = method,
    params = params,
    local_file_path = local_file_path,
    api_key = api_key
  )

  if (resolved$already_exists) {
    if (overwrite_strategy == "FAIL") {
      # check if files have equal content
      existing_file_checksum <- digest::digest(asset_file_path, algo = "sha1", serialize=FALSE, file = TRUE)
      downloaded_file_checksum <- digest::digest(local_file_path, algo = "sha1", serialize=FALSE, file = TRUE)

      # make sure to delete temporary file
      on.exit(file.remove(local_file_path))

      if (!identical(downloaded_file_checksum, existing_file_checksum)) {
        # raise an error because content is different
        comet_stop(
          sprintf("Cannot write Asset '%s' on path '%s', a file already exists.",
                  asset_filename, asset_file_path)
        )
      }
    } else {
      LOG_WARNING(
        sprintf("File '%s' has been overwritten by asset '%s'.",
                asset_file_path, asset_filename), echo = TRUE
      )
    }
  }
  create_asset_from_file(
    asset_file = asset_file_path,
    logical_path = asset_filename,
    overwrite = FALSE,
    metadata = metadata,
    asset_type = asset_type
  )
}

download_logged_artifact <- function(experiment_key,
                                     logged_artifact,
                                     path,
                                     overwrite_strategy,
                                     api_key = NULL) {
  artifact <- Artifact$new(
    artifact_name = logged_artifact$get_artifact_name(),
    artifact_type = logged_artifact$get_artifact_type(),
  )
  assets <- logged_artifact$get_assets()
  file_assets_num <-
    sum(unlist(sapply(assets, function(a) !a$is_remote())))

  LOG_INFO(
    sprintf(
      "Downloading %d assets of the Artifact '%s'",
      file_assets_num,
      create_full_artifact_name(
        artifact_name = logged_artifact$get_artifact_name(),
        workspace = logged_artifact$get_workspace(),
        version = as.character(logged_artifact$get_artifact_version())
      )
    ),
    echo = TRUE
  )

  for (asset in assets) {
    if (asset$is_remote()) {
      artifact_asset <- create_remote_asset(
        logical_path = asset$get_logical_path(),
        overwrite = asset$has_overwrite(),
        link = asset$get_link(),
        metadata = asset$get_metadata()
      )
    } else {
      artifact_asset <- asset$download(local_path = path,
                                       overwrite_strategy = overwrite_strategy)
    }
    # add asset
    artifact$add_asset(artifact_asset)
  }
  artifact
}
