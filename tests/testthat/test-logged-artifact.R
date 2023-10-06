test_that("LoggedArtifact return correct list of mocked assets when calling assets() and remote_assets()", {
  metadata_json <- encode_metadata(list(foo="bar"))
  files = list(
    list(
      remote = TRUE,
      fileName = "file1",
      fileSize = 0,
      link = "s3://test-bucket/dataset",
      metadata = metadata_json,
      type = "asset",
      assetId = generate_random_id()
    ),
    list(
      remote = FALSE,
      fileName = "dataset.dat",
      fileSize = 1110,
      link = NULL,
      metadata = metadata_json,
      type = "asset",
      assetId = generate_random_id()
    ),
    list(
      remote = FALSE,
      fileName = "dataset1.dat",
      fileSize = 1110,
      link = NULL,
      metadata = NULL,
      type = "asset",
      assetId = generate_random_id()
    )
  )
  local_mocked_bindings(
    get_artifact_files = function(...) {
      list(files = files)
    }
  )

  artifact_id = generate_random_id()
  artifact_version_id = generate_random_id()
  artifact <- LoggedArtifact$new(
    artifact_name = "Artifact",
    artifact_type = "dataset",
    artifact_id = artifact_id,
    artifact_version_id = artifact_version_id,
    workspace = "workspace",
    experiment_key = generate_random_id(),
    artifact_version = "1.1.1",
    aliases = list("one", "two"),
    artifact_tags = list("one", "two"),
    version_tags = list("one", "two"),
    size = 2220,
    metadata = list(foo = "bar")
  )

  assets = artifact$get_assets()
  expect_length(assets, 3)

  compared = mapply(function(expected, actual) {
    metadata_mathed = if (is.null(expected$metadata) &&
                          is.null(actual$get_metadata())) {
      TRUE
    } else {
      meta_json <- encode_metadata(actual$get_metadata())
      all.equal(expected$metadata, meta_json)
    }
    all(
      expected$fileName == actual$get_logical_path(),
      expected$remote == actual$is_remote(),
      expected$fileSize == actual$get_size(),
      expected$link == actual$get_link(),
      expected$type == actual$get_asset_type(),
      expected$assetId == actual$get_id(),
      metadata_mathed
    )
  }, files, assets)
  expect_true(all(compared))

  remote_assets = artifact$get_remote_assets()
  expect_length(remote_assets, 1)
  expect_true(remote_assets[[1]]$is_remote())
})
