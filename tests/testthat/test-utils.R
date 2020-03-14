test_that("custom OR works", {
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 5, 5)
  expect_equal(5 %||% NULL, 5)
  expect_equal(2 %||% 5, 2)
})

