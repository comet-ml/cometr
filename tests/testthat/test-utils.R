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
