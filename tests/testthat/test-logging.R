logfile <- R.utils::getAbsolutePath("cometr.log")
cleanup <- function() {
  if (file.exists(logfile)) {
    file.remove(logfile)
  }
  reset_comet_cache()
}

with_mock(
  `cometr:::get_config_logging_file` = function() logfile,
  {
    test_that("logging works at the right level", {
      with_mock(
        `cometr:::get_config_logging_file_level` = function() "DEBUG", {
          cleanup()
          LOG_DEBUG("test debug")
          LOG_INFO("test info")
          LOG_ERROR("test error")
          output <- readLines(logfile)

          expect_identical(length(output), 3L)
          expect_match(output[1], "\\[DBG\\] test debug")
          expect_match(output[2], "\\[INF\\] test info")
          expect_match(output[3], "\\[ERR\\] test error")
        }
      )

      with_mock(
        `cometr:::get_config_logging_file_level` = function() "INFO", {
          cleanup()
          LOG_DEBUG("test debug")
          LOG_INFO("test info")
          LOG_ERROR("test error")
          output <- readLines(logfile)

          expect_identical(length(output), 2L)
          expect_match(output[1], "\\[INF\\] test info")
          expect_match(output[2], "\\[ERR\\] test error")
        }
      )

      with_mock(
        `cometr:::get_config_logging_file_level` = function() "ERROR", {
          cleanup()
          LOG_DEBUG("test debug")
          LOG_INFO("test info")
          LOG_ERROR("test error")
          output <- readLines(logfile)

          expect_identical(length(output), 1L)
          expect_match(output[1], "\\[ERR\\] test error")
        }
      )

    })
  }
)
