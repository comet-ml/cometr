library(testthat)
library(cometr)

onCRAN <- function() !identical(Sys.getenv("NOT_CRAN"), "true")
if (!onCRAN()) {
  test_check("cometr")
}
