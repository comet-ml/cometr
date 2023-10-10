test_that("create_artifact works as expected", {
  aliases = list("one", "two", "three")
  tags = list("one", "two", "three")
  version = "1.2.3"
  artifact_name = "Artifact Name"
  artifact_type = "type"
  metadata = list(foo="bar")

  artifact <- create_artifact(artifact_name = artifact_name,
                              artifact_type = artifact_type,
                              artifact_version = version,
                              aliases = aliases,
                              metadata = metadata,
                              version_tags = tags)
  expect_identical(artifact$get_artifact_name(), artifact_name)
  expect_identical(artifact$get_artifact_type(), artifact_type)
  expect_identical(artifact$get_aliases(), list("one", "two", "three"))
  expect_identical(artifact$get_artifact_version(), numeric_version(version))
  expect_identical(artifact$get_version_tags(), list("one", "two", "three"))
  expect_identical(artifact$get_metadata(), metadata)
})

test_that("create_artifact with defaults works as expected", {
  artifact_name = "Artifact Name"
  artifact_type = "type"

  artifact <- create_artifact(artifact_name = artifact_name,
                              artifact_type = artifact_type)
  expect_identical(artifact$get_artifact_name(), artifact_name)
  expect_identical(artifact$get_artifact_type(), artifact_type)
  expect_null(artifact$get_aliases())
  expect_null(artifact$get_artifact_version())
  expect_null(artifact$get_version_tags())
  expect_null(artifact$get_metadata())
})

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
  aliases = list("one", "two", "three", "one")
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type", aliases = aliases)

  expect_identical(artifact$get_aliases(), list("one", "two", "three"))
})

test_that("Artifact constructor enforces list for aliases", {
  expect_error(
    Artifact$new(artifact_name = "test", artifact_type = "type", aliases = "not a list"),
    "aliases is not a list: not a list"
    )
})

test_that("Artifact constructor tags deduplicated", {
  tags = list("one", "two", "three", "one")
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type", version_tags = tags)

  expect_identical(artifact$get_version_tags(), list("one", "two", "three"))
})

test_that("Artifact constructor enforces list for version_tags", {
  expect_error(
    Artifact$new(artifact_name = "test", artifact_type = "type", version_tags = "not a list"),
    "version_tags is not a list: not a list"
  )
})

test_that("Artifact constructor version properly handled", {
  version = "1.2.3"
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type", artifact_version = version)

  r_version = artifact$get_artifact_version()
  expect_identical(r_version, numeric_version(version))
})

test_that("Artifact constructor invalid metadata raises error", {
  expect_error(
    Artifact$new(artifact_name = "test", artifact_type = "type", metadata = 34),
    "Invalid metadata, expecting list"
  )
})

test_that("Artifact add file assets works", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")
  local_path = test_path("test-data", "logo_dark.png")
  logical_path = "logo_image"
  metadata = list(foo="bar")

  artifact$add(local_path = local_path, overwrite = TRUE, logical_path = logical_path, metadata = metadata)

  assets = artifact$get_assets()
  expect_length(assets, 1)

  asset = assets[[1]]
  expect_identical(asset$get_local_path(), local_path)
  expect_identical(asset$get_logical_path(), logical_path)
  expect_identical(asset$get_metadata(), metadata)
  expect_true(asset$has_overwrite())
  expect_identical(asset$get_size(), 21113)
  expect_identical(asset$get_asset_type(), "asset")

  local_path = test_path("test-data", "test_audio.wav")
  artifact$add(local_path = local_path, overwrite = TRUE, metadata = metadata)

  assets = artifact$get_assets()
  expect_length(assets, 2)
})

test_that("Artifact add file asset without logical path works", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")
  local_path = test_path("test-data", "logo_dark.png")
  metadata = list(foo="bar")

  artifact$add(local_path = local_path, overwrite = TRUE, metadata = metadata)
  assets = artifact$get_assets()
  expect_length(assets, 1)

  asset = assets[[1]]
  expect_identical(asset$get_local_path(), local_path)
  expect_identical(asset$get_logical_path(), "logo_dark.png")
  expect_identical(asset$get_metadata(), metadata)
  expect_true(asset$has_overwrite())
})

test_that("Artifact add duplicate file asset raises error", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")
  local_path = test_path("test-data", "logo_dark.png")
  logical_path = "logo_image"
  metadata = list(foo="bar")

  artifact$add(local_path = local_path, logical_path = logical_path)

  assets = artifact$get_assets()
  expect_length(assets, 1)

  local_path = test_path("test-data", "test_audio.wav")
  message = sprintf("Cannot add new asset with logical_path '%s', an existing asset already exists with this logical_path. To add this asset to this artifact you should use a new unique logical_path.", logical_path)
  expect_error(
    artifact$add(local_path = local_path, logical_path = logical_path),
    message
  )
  expect_length(assets, 1)
})

test_that("Artifact add not existing file/dir raises error", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")
  local_path = "not_existing_file"

  expect_error(
    artifact$add(local_path = local_path),
    "local_path doesn't exists: not_existing_file"
  )
})

test_that("Artifact local path is NULL raises error", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")

  expect_error(
    artifact$add(local_path = NULL), "local_path can not be NULL")
})

test_that("Artifact add folder works with logical path", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")
  folder = test_path("test-data")
  logical_path = "dataset"
  metadata = list(foo="bar")

  artifact$add(local_path = folder, logical_path = logical_path,
               overwrite = TRUE, metadata = metadata)

  assets = artifact$get_assets()
  expect_length(assets, 4)

  for (asset in assets) {
    expect_match(asset$get_logical_path(), "^dataset")
    expect_gt(asset$get_size(), 0)
    expect_identical(asset$get_metadata(), metadata)
    expect_true(asset$has_overwrite())
    expect_identical(asset$get_asset_type(), "asset")
  }
})

test_that("Artifact add folder works without logical path", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")
  folder = test_path("test-data")
  metadata = list(foo="bar")

  artifact$add(local_path = folder, overwrite = TRUE, metadata = metadata)

  assets = artifact$get_assets()
  expect_length(assets, 4)

  for (asset in assets) {
    expect_match(asset$get_logical_path(), "^test-data")
    expect_gt(asset$get_size(), 0)
    expect_identical(asset$get_metadata(), metadata)
    expect_true(asset$has_overwrite())
    expect_identical(asset$get_asset_type(), "asset")
  }
})

test_that("Artifact add remote assets works", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")
  uri = "https://127.0.0.1/my_dataset.dat"
  metadata = list(foo="bar")
  logical_path = "dataset"

  artifact$add_remote(uri = uri, logical_path = logical_path, overwrite = TRUE, metadata = metadata)

  assets = artifact$get_assets()
  expect_length(assets, 1)

  asset = assets[[1]]
  expect_identical(asset$get_link(), uri)
  expect_identical(asset$get_logical_path(), logical_path)
  expect_identical(asset$get_metadata(), metadata)
  expect_true(asset$has_overwrite())
  expect_identical(asset$get_size(), 0)
  expect_identical(asset$get_asset_type(), "asset")
  expect_null(asset$get_local_path())

  uri = "https://127.0.0.1/my_dataset_2.dat"
  logical_path = "dataset_2"

  artifact$add_remote(uri = uri, logical_path = logical_path, overwrite = TRUE, metadata = metadata)

  assets = artifact$get_assets()
  expect_length(assets, 2)
})

test_that("Artifact add remote assets with empty URI raise an error", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")

  expect_error(artifact$add_remote(uri = NULL), "uri can not be NULL")
})

test_that("Artifact add remote assets works without logical path", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")
  uri = "https://127.0.0.1/my_dataset.dat"
  metadata = list(foo="bar")

  artifact$add_remote(uri = uri, overwrite = TRUE, metadata = metadata)

  assets = artifact$get_assets()
  expect_length(assets, 1)

  asset = assets[[1]]
  expect_identical(asset$get_link(), uri)
  expect_identical(asset$get_logical_path(), "my_dataset.dat")
  expect_identical(asset$get_metadata(), metadata)
  expect_true(asset$has_overwrite())
  expect_identical(asset$get_size(), 0)
  expect_identical(asset$get_asset_type(), "asset")
  expect_null(asset$get_local_path())
})

test_that("Artifact add duplicate remote asset raises error", {
  artifact = Artifact$new(artifact_name = "test", artifact_type = "type")
  uri = "https://127.0.0.1/my_dataset.dat"

  artifact$add_remote(uri = uri)

  assets = artifact$get_assets()
  expect_length(assets, 1)

  logical_path = "my_dataset.dat"
  message = sprintf("Cannot add new asset with logical_path '%s', an existing asset already exists with this logical_path. To add this asset to this artifact you should use a new unique logical_path.", logical_path)
  expect_error(artifact$add_remote(uri = uri), message)
})
