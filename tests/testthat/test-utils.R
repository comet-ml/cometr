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
      307,
      856,
      1205,
      1038,
      5940,
      250,
      3940,
      328,
      1593,
      53938,
      59061210,
      3750711,
      16251,
      42756,
      6709,
      19697,
      13230426
    )
  expected <-
    c(
      "255B",
      "307B",
      "856B",
      "1.18KB",
      "1.01KB",
      "5.8KB",
      "250B",
      "3.85KB",
      "328B",
      "1.56KB",
      "52.67KB",
      "56.33MB",
      "3.58MB",
      "15.87KB",
      "41.75KB",
      "6.55KB",
      "19.24KB",
      "12.62MB"
    )

  res <- sapply(sizes, file_size_formated)
  expect_identical(res, expected)
})
