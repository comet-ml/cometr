test_that("Artifact constructor enforces mandatory fields", {
  expect_error(
    Artifact$new(artifact_name = NULL, artifact_type = "type"),
    "Artifact name is mandatory"
  )
  expect_error(
    Artifact$new(artifact_name = "test", artifact_type = NULL),
    "Artifact type is mandatory"
  )
})

test_that("Artifact constructor aliases deduplicated", {
  aliases = c("one", "two", "three", "one")
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type", aliases = aliases)

  testthat::expect_identical(artifact$get_aliases(), c("one", "two", "three"))
})

test_that("Artifact constructor tags deduplicated", {
  tags = c("one", "two", "three", "one")
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type", version_tags = tags)

  testthat::expect_identical(artifact$get_version_tags(), c("one", "two", "three"))
})

test_that("Artifact constructor version properly handled", {
  version = "1.2.3"
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type", artifact_version = version)

  r_version = artifact$get_artifact_version()
  testthat::expect_identical(r_version, numeric_version(version))
})

test_that("Artifact constructor invalid metadata raise error", {
  expect_error(
    Artifact$new(artifact_name = "test", artifact_type = "type", metadata = 34),
    "Invalid metadata, expecting list"
  )
})

test_that("Artifact add file assets", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")
  local_path = "./test-data/logo_dark.png"
  logical_path = "logo_image"
  metadata = list(foo="bar")

  artifact$add(local_path = local_path, overwrite = TRUE, logical_path = logical_path, metadata = metadata)

  assets = artifact$get_assets()
  testthat::expect_length(assets, 1)

  asset = assets[[1]]
  testthat::expect_identical(asset$get_local_path(), local_path)
  testthat::expect_identical(asset$get_logical_path(), logical_path)
  testthat::expect_identical(asset$get_metadata(), metadata)
  testthat::expect_true(asset$has_overwrite())
  testthat::expect_identical(asset$get_size(), 21113)
  testthat::expect_identical(asset$get_asset_type(), "asset")

  local_path = "./test-data/test_audio.wav"
  artifact$add(local_path = local_path, overwrite = TRUE, metadata = metadata)

  assets = artifact$get_assets()
  testthat::expect_length(assets, 2)
})

test_that("Artifact add file asset without logical path", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")
  local_path = "./test-data/logo_dark.png"
  metadata = list(foo="bar")

  artifact$add(local_path = local_path, overwrite = TRUE, metadata = metadata)
  assets = artifact$get_assets()
  testthat::expect_length(assets, 1)

  asset = assets[[1]]
  testthat::expect_identical(asset$get_local_path(), local_path)
  testthat::expect_identical(asset$get_logical_path(), "logo_dark.png")
  testthat::expect_identical(asset$get_metadata(), metadata)
  testthat::expect_true(asset$has_overwrite())
})

test_that("Artifact add file asset duplicate", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")
  local_path = "./test-data/logo_dark.png"
  logical_path = "logo_image"
  metadata = list(foo="bar")

  artifact$add(local_path = local_path, logical_path = logical_path)

  assets = artifact$get_assets()
  testthat::expect_length(assets, 1)

  local_path = "./test-data/test_audio.wav"
  message = sprintf("Cannot add new asset with logical_path '%s', an existing asset already exists with this logical_path. To add this asset to this artifact you should use a new unique logical_path.", logical_path)
  testthat::expect_error(
    artifact$add(local_path = local_path, logical_path = logical_path),
    message
  )
  testthat::expect_length(assets, 1)
})

test_that("Artifact add not existing file/dir", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")
  local_path = "not_existing_file"

  testthat::expect_error(
    artifact$add(local_path = local_path),
    "local_path doesn't exists: not_existing_file"
  )
})

test_that("Artifact local path is NULL", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")

  testthat::expect_error(
    artifact$add(local_path = NULL), "local_path can not be NULL")
})

test_that("Artifact add folder works with logical path", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")
  folder = "./test-data"
  logical_path = "dataset"
  metadata = list(foo="bar")

  artifact$add(local_path = folder, logical_path = logical_path,
               overwrite = TRUE, metadata = metadata)

  assets = artifact$get_assets()
  testthat::expect_length(assets, 3)

  for (asset in assets) {
    testthat::expect_match(asset$get_logical_path(), "^dataset")
    testthat::expect_gt(asset$get_size(), 0)
    testthat::expect_identical(asset$get_metadata(), metadata)
    testthat::expect_true(asset$has_overwrite())
    testthat::expect_identical(asset$get_asset_type(), "asset")
  }
})

test_that("Artifact add folder works without logical path", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")
  folder = "./test-data"
  metadata = list(foo="bar")

  artifact$add(local_path = folder, overwrite = TRUE, metadata = metadata)

  assets = artifact$get_assets()
  testthat::expect_length(assets, 3)

  for (asset in assets) {
    testthat::expect_match(asset$get_logical_path(), "^test-data")
    testthat::expect_gt(asset$get_size(), 0)
    testthat::expect_identical(asset$get_metadata(), metadata)
    testthat::expect_true(asset$has_overwrite())
    testthat::expect_identical(asset$get_asset_type(), "asset")
  }
})
