if (hasInternet()) {

  new_exp_name <- paste0("exp-artifacts-", generate_random_id())
  test_exp <- create_experiment(experiment_name = new_exp_name, project_name = test_proj,
                                api_key = test_api_key, keep_active = FALSE,
                                log_output = TRUE, log_error = FALSE, log_code = FALSE,
                                log_system_details = FALSE, log_git_info = FALSE)


  # test_that("Artifact can be logged by experiment with all assets", {
  #   artifact_name <- paste0("artifact-", generate_random_id())
  #   metadata <- list(foo="bar")
  #   tags <- list("tag1", "tag2")
  #   aliases <- list("alias1", "alias2")
  #   remote_uri <- "http://localhost/dataset.dat"
  #   artifact <- create_artifact(artifact_name = artifact_name,
  #                               artifact_type = "dataset",
  #                               metadata = metadata,
  #                               aliases = aliases,
  #                               version_tags = tags)
  #   artifact$add_remote(uri = remote_uri,
  #                       logical_path = "dataset",
  #                       metadata = metadata)
  #   artifact$add(local_path = test_path("test-data"),
  #                logical_path = "test-files",
  #                metadata = metadata)
  #   artifact$add(local_path = test_path("test-data", "sample-script.R"),
  #                logical_path = "samle-code",
  #                metadata = metadata)
  #
  #   logged_artifact <- test_exp$log_artifact(artifact)
  #
  #   # check that returned logged artifact has appropriate fields initialized
  #   expect_equal(logged_artifact$get_artifact_name(), artifact_name)
  #   expect_equal(logged_artifact$get_artifact_type(), "dataset")
  #
  #   expect_true(!is.null(logged_artifact$get_artifact_id()))
  #   expect_true(!is.null(logged_artifact$get_artifact_version_id()))
  #
  #   expect_equal(logged_artifact$get_source_experiment_key(), test_exp$get_key())
  #   expect_equal(logged_artifact$get_experiment_key(), test_exp$get_key())
  #
  #   expect_equal(logged_artifact$get_artifact_version(), numeric_version("1.0.0"))
  #   expect_equal(logged_artifact$get_version_tags(), tags)
  #
  #   expect_equal(logged_artifact$get_metadata(), metadata)
  #   expect_equal(logged_artifact$get_workspace(), test_exp$get_workspace_name())
  #
  #   expect_true(logged_artifact$size() > 0)
  #
  #   expect_equal(logged_artifact$get_aliases(), aliases)
  #
  #   remote_assets <- logged_artifact$get_remote_assets()
  #   expect_length(remote_assets, 1)
  #   expect_true(remote_assets[[1]]$is_remote())
  #   expect_equal(remote_assets[[1]]$get_link(), remote_uri)
  #   expect_equal(remote_assets[[1]]$get_logical_path(), "dataset")
  #   expect_equal(remote_assets[[1]]$get_metadata(), metadata)
  #
  #   assets <- logged_artifact$get_assets()
  #   expect_length(assets, 6)
  #
  #   expected_logical_paths <- list("samle-code",
  #                                  "test-files/logo_dark.png",
  #                                  "test-files/sample-script.R",
  #                                  "test-files/test_audio.wav",
  #                                  "test-files/test_table.csv")
  #   for (asset in assets) {
  #     if (!asset$is_remote()) {
  #       expect_in(asset$get_logical_path(), expected_logical_paths)
  #       expect_equal(asset$get_metadata(), metadata)
  #     }
  #   }
  # })

  test_that("Artifact can be logged with defined version", {
    artifact_name <- paste0("artifact-", generate_random_id())
    version <- "2.3.1"
    artifact <- create_artifact(artifact_name = artifact_name,
                                artifact_type = "version-test",
                                artifact_version = version)

    logged_artifact <- test_exp$log_artifact(artifact)
    expect_equal(logged_artifact$get_artifact_version(), numeric_version(version))
  })


  # test_exp$delete()
}
