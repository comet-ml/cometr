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
