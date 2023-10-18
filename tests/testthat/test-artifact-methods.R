if (hasInternet()) {

  new_exp_name <- paste0("exp-artifacts-", generate_random_id())
  test_exp <- create_experiment(experiment_name = new_exp_name, project_name = test_proj,
                                api_key = test_api_key, keep_active = FALSE,
                                log_output = TRUE, log_error = FALSE, log_code = FALSE,
                                log_system_details = FALSE, log_git_info = FALSE)


  test_that("Artifact can be logged by experiment with all assets", {
    artifact_name <- paste0("artifact-", generate_random_id())
    metadata <- list(foo="bar")
    tags <- list("tag1", "tag2")
    aliases <- list("alias1", "alias2")
    remote_uri <- "http://localhost/dataset.dat"
    artifact <- create_artifact(artifact_name = artifact_name,
                                artifact_type = "dataset",
                                metadata = metadata,
                                aliases = aliases,
                                version_tags = tags)
    artifact$add_remote(uri = remote_uri,
                        logical_path = "dataset",
                        metadata = metadata)
    artifact$add(local_path = test_path("test-data"),
                 logical_path = "test-files",
                 metadata = metadata)
    artifact$add(local_path = test_path("test-data", "sample-script.R"),
                 logical_path = "samle-code",
                 metadata = metadata)

    logged_artifact <- test_exp$log_artifact(artifact)

    # check that returned logged artifact has appropriate fields initialized
    expect_equal(logged_artifact$get_artifact_name(), artifact_name)
    expect_equal(logged_artifact$get_artifact_type(), "dataset")

    expect_true(!is.null(logged_artifact$get_artifact_id()))
    expect_true(!is.null(logged_artifact$get_artifact_version_id()))

    expect_equal(logged_artifact$get_source_experiment_key(), test_exp$get_key())
    expect_equal(logged_artifact$get_experiment_key(), test_exp$get_key())

    expect_equal(logged_artifact$get_artifact_version(), numeric_version("1.0.0"))
    expect_equal(logged_artifact$get_version_tags(), tags)

    expect_equal(logged_artifact$get_metadata(), metadata)
    expect_equal(logged_artifact$get_workspace(), test_exp$get_workspace_name())

    expect_true(logged_artifact$size() > 0)

    expect_equal(logged_artifact$get_aliases(), aliases)

    remote_assets <- logged_artifact$get_remote_assets()
    expect_length(remote_assets, 1)
    expect_true(remote_assets[[1]]$is_remote())
    expect_equal(remote_assets[[1]]$get_link(), remote_uri)
    expect_equal(remote_assets[[1]]$get_logical_path(), "dataset")
    expect_equal(remote_assets[[1]]$get_metadata(), metadata)

    assets <- logged_artifact$get_assets()
    expect_length(assets, 6)

    expected_logical_paths <- list("samle-code",
                                   "test-files/logo_dark.png",
                                   "test-files/sample-script.R",
                                   "test-files/test_audio.wav",
                                   "test-files/test_table.csv")
    for (asset in assets) {
      if (!asset$is_remote()) {
        expect_in(asset$get_logical_path(), expected_logical_paths)
        expect_equal(asset$get_metadata(), metadata)
      }
    }
  })

  test_that("Artifact version can be added", {
    artifact_name <- paste0("artifact-", generate_random_id())
    artifact <- create_artifact(artifact_name = artifact_name,
                                artifact_type = "add-version-test")
    logged_artifact <- test_exp$log_artifact(artifact)
    expect_equal(logged_artifact$get_artifact_version(), numeric_version("1.0.0"))

    # create new version of artifact
    version <- "3.3.1"
    metadata <- list(foo="bar")
    remote_uri <- "http://localhost/dataset.dat"
    artifact <- create_artifact(artifact_name = artifact_name,
                                artifact_type = "add-version-test",
                                artifact_version = version )
    artifact$add_remote(uri = remote_uri,
                        logical_path = "dataset",
                        metadata = metadata)
    test_exp$log_artifact(artifact)

    # get artifact and check version
    logged_artifact <- get_artifact_by_name(
      artifact_name = artifact_name,
      experiment_key = test_exp$get_key()
    )
    expect_equal(logged_artifact$get_artifact_version(), numeric_version(version))

    remote_assets <- logged_artifact$get_remote_assets()
    expect_length(remote_assets, 1)
    expect_true(remote_assets[[1]]$is_remote())
    expect_equal(remote_assets[[1]]$get_link(), remote_uri)
    expect_equal(remote_assets[[1]]$get_logical_path(), "dataset")
    expect_equal(remote_assets[[1]]$get_metadata(), metadata)
  })

  test_that("Artifact can be logged with defined version", {
    artifact_name <- paste0("artifact-", generate_random_id())
    version <- "2.3.1"
    artifact <- create_artifact(artifact_name = artifact_name,
                                artifact_type = "version-test",
                                artifact_version = version)

    logged_artifact <- test_exp$log_artifact(artifact)
    expect_equal(logged_artifact$get_artifact_version(), numeric_version(version))
  })

  test_that("Artifact can be retrieved by name", {
    artifact_name <- paste0("artifact-", generate_random_id())
    version <- "2.3.1"
    aliases <- list("alias1", "alias2")
    workspace <- test_exp$get_workspace_name()
    artifact <- create_artifact(artifact_name = artifact_name,
                                artifact_type = "by_name-test",
                                artifact_version = version,
                                aliases = aliases)

    logged_artifact <- test_exp$log_artifact(artifact)
    expect_equal(logged_artifact$get_artifact_version(), numeric_version(version))
    artifact_id <- logged_artifact$get_artifact_id()
    artifact_version_id <- logged_artifact$get_artifact_version_id()

    # get artifact by full name and check results
    name_string <- paste0(workspace, "/", artifact_name, ":", version)
    name_strings <- list(
      paste0(workspace, "/", artifact_name, ":", version),
      paste0(artifact_name, ":", version),
      artifact_name,
      paste0(workspace, "/", artifact_name, ":", "alias1"),
      paste0(artifact_name, ":", "alias2")
    )

    for (name_string in name_strings) {
      logged_artifact <- get_artifact_by_name(
        artifact_name = name_string,
        experiment_key = test_exp$get_key()
      )
      expect_equal(logged_artifact$get_artifact_id(), artifact_id)
      expect_equal(logged_artifact$get_artifact_version_id(), artifact_version_id)
    }

    # get artifact by explicit parameters
    logged_artifact <- get_artifact_by_name(
      artifact_name = artifact_name,
      workspace = workspace,
      version_or_alias = version,
      experiment_key = test_exp$get_key()
    )
    expect_equal(logged_artifact$get_artifact_id(), artifact_id)
    expect_equal(logged_artifact$get_artifact_version_id(), artifact_version_id)

    logged_artifact <- get_artifact_by_name(
      artifact_name = artifact_name,
      workspace = workspace,
      experiment_key = test_exp$get_key()
    )
    expect_equal(logged_artifact$get_artifact_id(), artifact_id)
    expect_equal(logged_artifact$get_artifact_version_id(), artifact_version_id)

    logged_artifact <- get_artifact_by_name(
      artifact_name = artifact_name,
      experiment_key = test_exp$get_key()
    )
    expect_equal(logged_artifact$get_artifact_id(), artifact_id)
    expect_equal(logged_artifact$get_artifact_version_id(), artifact_version_id)
  })

  test_exp$delete()
}
