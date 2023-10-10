# clean any existing log files if any
cleanup()

with_mock(
  `cometr:::get_config_logging_file` = function() logfile, {
    test_that("logging works at the right level: DEBUG", {
      with_mock(
        `cometr:::get_config_logging_file_level` = function() "DEBUG", {
          on.exit(cleanup())

          LOG_DEBUG("test debug")
          LOG_INFO("test info")
          LOG_WARNING("test warning")
          LOG_ERROR("test error")
          output <- readLines(logfile)

          expect_identical(length(output), 4L)
          expect_match(output[1], "\\[DBG\\] test debug")
          expect_match(output[2], "\\[INF\\] test info")
          expect_match(output[3], "\\[WARN\\] test warning")
          expect_match(output[4], "\\[ERR\\] test error")
        }
      )
    })

    test_that("logging works at the right level: INFO", {
      with_mock(
        `cometr:::get_config_logging_file_level` = function() "INFO", {
          on.exit(cleanup())

          LOG_DEBUG("test debug")
          LOG_INFO("test info")
          LOG_WARNING("test warning")
          LOG_ERROR("test error")
          output <- readLines(logfile)

          expect_identical(length(output), 3L)
          expect_match(output[1], "\\[INF\\] test info")
          expect_match(output[2], "\\[WARN\\] test warning")
          expect_match(output[3], "\\[ERR\\] test error")
        }
      )
    })

    test_that("logging works at the right level: WARNING", {
      with_mock(
        `cometr:::get_config_logging_file_level` = function() "WARNING", {
          on.exit(cleanup())

          LOG_DEBUG("test debug")
          LOG_INFO("test info")
          LOG_WARNING("test warning")
          LOG_ERROR("test error")
          output <- readLines(logfile)

          expect_identical(length(output), 2L)
          expect_match(output[1], "\\[WARN\\] test warning")
          expect_match(output[2], "\\[ERR\\] test error")
        }
      )
    })

    test_that("logging works at the right level: ERROR", {
      with_mock(
        `cometr:::get_config_logging_file_level` = function() "ERROR", {
          on.exit(cleanup())

          LOG_DEBUG("test debug")
          LOG_INFO("test info")
          LOG_WARNING("test warning")
          LOG_ERROR("test error")
          output <- readLines(logfile)

          expect_identical(length(output), 1L)
          expect_match(output[1], "\\[ERR\\] test error")
        }
      )
    })
  }
)

test_that("logging gets canceled and errors correctly when appropriate", {

  with_mock(
    `cometr:::get_config_logging_file` = function() NULL,
    `cometr:::get_config_logging_file_level` = function() NULL, {
      on.exit(cleanup())
      expect_false(can_write_log_helper())
    }
  )

  with_mock(
    `cometr:::get_config_logging_file` = function() NULL,
    `cometr:::get_config_logging_file_level` = function() "DEBUG", {
      on.exit(cleanup())
      expect_warning(value <- can_write_log_helper())
      expect_false(value)
    }
  )

  with_mock(
    `cometr:::get_config_logging_file` = function() logfile,
    `cometr:::get_config_logging_file_level` = function() NULL, {
      on.exit(cleanup())
      expect_warning(value <- can_write_log_helper())
      expect_false(value)
    }
  )

  with_mock(
    `cometr:::get_config_logging_file` = function() logfile,
    `cometr:::get_config_logging_file_level` = function() "DEBUG", {
      on.exit(cleanup())
      expect_true(can_write_log_helper())
    }
  )

  with_mock(
    `cometr:::get_config_logging_file` = function() logfile,
    `cometr:::get_config_logging_file_level` = function() "DEBUG", {
      on.exit(cleanup())

      expect_true(can_write_log())
      cleanup()
      disable_logging()
      expect_false(can_write_log())
      cleanup()
      expect_true(can_write_log())
    }
  )

  with_mock(
    `cometr:::get_config_logging_file` = function() logfile,
    `cometr:::get_config_logging_file_level` = function() "NOSUCHLEVEL", {
      on.exit(cleanup())
      expect_warning(value <- can_write_log_helper())
      expect_false(value)
    }
  )

  with_mock(
    file.create = function(..., showWarnings = TRUE) stop("can't create file"),
    `cometr:::get_config_logging_file` = function() logfile,
    `cometr:::get_config_logging_file_level` = function() "DEBUG", {
      on.exit(cleanup())
      expect_warning(value <- can_write_log_helper())
      expect_false(value)
    }
  )

  with_mock(
    `cometr:::get_config_logging_file` = function() logfile,
    `cometr:::get_config_logging_file_level` = function() "DEBUG", {
      on.exit(cleanup())
      expect_true(can_write_log_helper())
      Sys.chmod(logfile, mode = "0000")
      expect_warning(value <- can_write_log_helper())
      expect_false(value)
      Sys.chmod(logfile, mode = "0777")
      expect_true(can_write_log_helper())
  })

})

with_mock(
  `cometr:::can_write_log` = function() TRUE,
  `cometr:::get_config_logging_file` = function() logfile,
  `cometr:::get_config_logging_file_level` = function() "DEBUG", {
    test_that("Cannot log a level other than INFO, DEBUG, WARNING, ERROR", {
      on.exit(cleanup())
      expect_warning(comet_log("test", level = 0))
      expect_warning(comet_log("test", level = 1), NA)
      expect_warning(comet_log("test", level = 2), NA)
      expect_warning(comet_log("test", level = 3), NA)
      expect_warning(comet_log("test", level = 4), NA)
      expect_warning(comet_log("test", level = 5))
    })
  }
)
