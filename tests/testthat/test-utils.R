test_that("custom OR works", {
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 5, 5)
  expect_equal(5 %||% NULL, 5)
  expect_equal(2 %||% 5, 2)
})

test_that("isBool works", {
  expect_true(isBool(TRUE))
  expect_true(isBool(FALSE))
  expect_false(isBool(NA))
  expect_false(isBool(NULL))
  expect_false(isBool(5))
  expect_false(isBool("test"))
  expect_false(isBool(0))
  expect_false(isBool(""))
})

test_that("get_values_from_list works", {
  nested_list <- list(list(a="a", b="b"), list(a="A", b="B"))
  expect_identical(get_values_from_list(nested_list, "a"), c("a", "A"))
  expect_identical(get_values_from_list(nested_list, "b"), c("b", "B"))
  expect_null(get_values_from_list(nested_list, "c"))
})

test_that("file_size_formated works", {
  sizes <-
    c(
      255,
      19697,
      13230426,
      132304261234,
      13230426123412
    )
  expected <-
    c(
      "255B",
      "19.24KB",
      "12.62MB",
      "123.22GB",
      "12.03TB"
    )

  res <- sapply(sizes, file_size_formated)
  expect_identical(res, expected)
})

test_that("remote_asset_name_from_uri works", {
  uris <-
    list("s3://bucket/dataset.dat",
         "https://localhost:8080/file.txt",
         "https://comet.com/dataset",
         NULL,
         NA,
         "file.dat")
  expected <-
    list("dataset.dat",
         "file.txt",
         "dataset",
         "remote",
         "remote",
         "file.dat")
  res <- sapply(uris, remote_asset_name_from_uri)
  expect_identical(res, expected)
})

test_that("parse_artifact_name works", {
  names <- list(
    "workspace/artifact-name:versionOrAlias",
    "workspace/artifact-name",
    "artifact-name:versionOrAlias",
    "artifact-name"
  )
  expected <- list(
      "workspace", "artifact-name", "versionOrAlias",
      "workspace", "artifact-name", NULL,
      NULL, "artifact-name", "versionOrAlias",
      NULL, "artifact-name", NULL
  )
  res <- sapply(names, parse_artifact_name)
  expect_identical(unlist(res), unlist(expected))
})

test_that("encode_metadata and decode_metadata works", {
  metadata <- list(foo="bar")

  encoded <- encode_metadata(metadata)
  decoded <- decode_metadata(encoded)
  expect_equal(decoded, metadata)
})
