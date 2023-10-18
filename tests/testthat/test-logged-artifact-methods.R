if (hasInternet()) {

  new_exp_name <- paste0("exp-logged-artifacts-", generate_random_id())
  test_exp <- create_experiment(experiment_name = new_exp_name, project_name = test_proj,
                                api_key = test_api_key, keep_active = FALSE,
                                log_output = TRUE, log_error = FALSE, log_code = FALSE,
                                log_system_details = FALSE, log_git_info = FALSE)

  test_that("LoggedArtifact update_artifact_tags works", {
    artifact_name <- paste0("artifact-", generate_random_id())
    version <- "3.3.1"
    tags <- list("tag1", "tag2")
    artifact <- create_artifact(artifact_name = artifact_name,
                                artifact_type = "update-tags-test",
                                artifact_version = version,
                                version_tags = tags)
    logged_artifact <- test_exp$log_artifact(artifact)

    new_tags <- list("tag3", "tag4")
    logged_artifact$update_artifact_tags(new_tags)

    # get artifact and check tags
    logged_artifact <- get_artifact_by_name(
      artifact_name = artifact_name,
      experiment_key = test_exp$get_key()
    )
    expect_equal(logged_artifact$get_artifact_tags(), new_tags)
  })

  test_that("LoggedArtifact update_version_tags works", {
    artifact_name <- paste0("artifact-", generate_random_id())
    version <- "2.3.1"
    tags <- list("tag1", "tag2")
    artifact <- create_artifact(artifact_name = artifact_name,
                                artifact_type = "update-version-tags-test",
                                artifact_version = version,
                                version_tags = tags)
    logged_artifact <- test_exp$log_artifact(artifact)

    new_tags <- list("tag3", "tag4")
    logged_artifact$update_version_tags(new_tags)

    # get artifact and check version tags
    logged_artifact <- get_artifact_by_name(
      artifact_name = artifact_name,
      experiment_key = test_exp$get_key()
    )
    expect_equal(logged_artifact$get_version_tags(), new_tags)
  })

  test_that("LoggedArtifact update_aliases works", {
    artifact_name <- paste0("artifact-", generate_random_id())
    version <- "2.1.1"
    aliases <- list("alias1", "alias2")
    artifact <- create_artifact(artifact_name = artifact_name,
                                artifact_type = "update-version-aliases-test",
                                artifact_version = version,
                                aliases = aliases)
    logged_artifact <- test_exp$log_artifact(artifact)

    new_aliases <- list("alias3", "alias4")
    logged_artifact$update_aliases(new_aliases)

    # get artifact and check version aliases
    logged_artifact <- get_artifact_by_name(
      artifact_name = artifact_name,
      experiment_key = test_exp$get_key()
    )
    expect_equal(logged_artifact$get_aliases(), new_aliases)
  })

  test_that("LoggedArtifactAsset download works", {
    # move assets to temporary dir
    parent_dir <- withr::local_tempdir()
    file.copy(test_path("test-data"), parent_dir, recursive = TRUE)

    test_data_tmp <- file.path(parent_dir, "test-data")
    asset_files <- list.files(test_data_tmp,
                             recursive = TRUE,
                             full.names = FALSE)
    expect_length(asset_files, 4)

    # create and log artifact
    artifact_name <- paste0("artifact-", generate_random_id())
    metadata <- list(foo="bar")
    remote_uri <- "http://localhost/dataset.dat"
    artifact <- create_artifact(artifact_name = artifact_name,
                                artifact_type = "dataset")
    artifact$add_remote(uri = remote_uri,
                        logical_path = "dataset",
                        metadata = metadata)
    artifact$add(local_path = test_data_tmp,
                 metadata = metadata)
    artifact$add(local_path = file.path(test_data_tmp, "sample-script.R"),
                 logical_path = "sample-code",
                 metadata = metadata)

    logged_artifact <- test_exp$log_artifact(artifact)

    assets <- logged_artifact$get_assets()
    expect_length(assets, 6)

    # check FAIL with equal files - no error
    #
    asset_filename <- "test-data/sample-script.R"
    asset_path <- file.path(test_data_tmp, "sample-script.R")
    asset <- asset_by_name(assets, logical_path = asset_filename)[[1]]
    artifact_asset <- asset$download(local_path = parent_dir)

    expect_equal(artifact_asset$get_local_path(), asset_path)
    expect_equal(artifact_asset$get_size(), file.size(asset_path))
    expect_equal(artifact_asset$get_metadata(), metadata)
    expect_equal(artifact_asset$get_logical_path(), asset_filename)

    # check FAIL with not equal files - error must be raised
    #
    cat("# Modified data", file = asset_path, append = TRUE)
    expect_error(asset$download(local_path = parent_dir),
                 "a file already exists.")

    # check OVERWRITE
    #
    asset_filename <- "test-data/test_table.csv"
    asset_path <- file.path(test_data_tmp, "test_table.csv")
    asset_size_before <- file.size(asset_path)
    asset <-
      asset_by_name(assets, logical_path = asset_filename)[[1]]
    artifact_asset <-
      asset$download(local_path = parent_dir, overwrite_strategy = "OVERWRITE")

    expect_equal(asset_size_before, file.size(asset_path))
    expect_equal(artifact_asset$get_local_path(), asset_path)
    expect_equal(artifact_asset$get_metadata(), metadata)
    expect_equal(artifact_asset$get_logical_path(), asset_filename)

    # check PRESERVE
    #
    asset_filename <- "test-data/logo_dark.png"
    asset_path <- file.path(test_data_tmp, "logo_dark.png")
    asset <- asset_by_name(assets, logical_path = asset_filename)[[1]]

    artifact_asset <- expect_output(
      asset$download(local_path = parent_dir, overwrite_strategy = "PRESERVE"),
      "due to selected overwrite strategy.")

    expect_equal(artifact_asset$get_local_path(), asset_path)
    expect_equal(artifact_asset$get_size(), file.size(asset_path))
    expect_equal(artifact_asset$get_metadata(), metadata)
    expect_equal(artifact_asset$get_logical_path(), asset_filename)

    # check remote asset download - raises an error
    #
    logical_path <-  "dataset"
    asset <- asset_by_name(assets, logical_path = logical_path)[[1]]

    expect_error(asset$download(local_path = parent_dir),
                 "Failed to download remote asset")

  })

  test_that("LoggedArtifact download works", {
    # move assets to temporary dir
    parent_dir <- withr::local_tempdir()
    file.copy(test_path("test-data"), parent_dir, recursive = TRUE)

    test_data_tmp <- file.path(parent_dir, "test-data")
    asset_files <- list.files(test_data_tmp,
                             recursive = TRUE,
                             full.names = FALSE)
    expect_length(asset_files, 4)

    # create and log artifact
    artifact_name <- paste0("artifact-", generate_random_id())
    metadata <- list(foo="bar")
    remote_uri <- "http://localhost/dataset.dat"
    artifact <- create_artifact(artifact_name = artifact_name,
                                artifact_type = "dataset")
    artifact$add_remote(uri = remote_uri,
                        logical_path = "dataset",
                        metadata = metadata)
    artifact$add(local_path = test_data_tmp,
                 metadata = metadata)
    artifact$add(local_path = file.path(test_data_tmp, "sample-script.R"),
                 logical_path = "sample-code",
                 metadata = metadata)

    logged_artifact <- test_exp$log_artifact(artifact)

    assets <- logged_artifact$get_assets()
    expect_length(assets, 6)

    # Download assets to temporary dir and validate
    #
    download_dir <- file.path(parent_dir, "download_dir")
    artifact <- logged_artifact$download(
      path = download_dir, overwrite_strategy = FALSE
    )
    artifact_assets <- artifact$get_assets()
    expect_length(artifact_assets, 6)

    downloaded_asset_files <- list.files(download_dir,
                              recursive = TRUE,
                              full.names = FALSE)
    expect_length(downloaded_asset_files, 5)

    expected_names <- unlist(sapply(asset_files, function(f) basename(f), USE.NAMES = FALSE))
    expected_names <- sort(append(expected_names, "sample-code"))
    names <- sort(unlist(sapply(downloaded_asset_files, function(f) basename(f), USE.NAMES = FALSE)))

    expect_equal(names, expected_names)

    expect_equal(artifact$get_artifact_name(), artifact_name)
    expect_equal(artifact$get_artifact_type(), "dataset")

  })

  test_exp$delete()
}
