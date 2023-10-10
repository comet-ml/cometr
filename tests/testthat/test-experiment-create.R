test_that("cannot create an Experiment object directly", {
  expect_error(Experiment$new(), "Do not call this function directly.")
})

test_that("creating a second experiment causes the first to stop", {
  on.exit(reset_comet_cache())

  expect_output(
    exp1 <- mock_experiment_full(),
    "^Experiment created"
  )
  exp1$stop()
  expect_output(
    exp2 <- mock_experiment_full(),
    "^Experiment created"
  )
  exp2$stop()

  expect_output(
    exp1 <- mock_experiment_full(),
    "^Experiment created"
  )
  expect_output(
    exp2 <- mock_experiment_full(),
    "^Existing experiment.*Experiment created"
  )
  exp2$stop()
})

test_that("a stopped experiment cannot be modified", {
  with_mock(
    `cometr:::add_tags` = function(...) NULL, {
      exp <- mock_experiment_by_id("test1")
      exp$add_tags("tag1")
      exp$stop()
      expect_error(
        exp$add_tags("tag1"),
        "cannot be modified"
      )
    }
  )
})

test_that("logging process is alive until the experiment stops", {
  exp <- mock_experiment_full(experiment_key = test_exp_id, log_output = FALSE)
  Sys.sleep(1)
  expect_null(exp$.__enclos_env__$private$logging_process)
  exp$stop()

  exp <- mock_experiment_full(experiment_key = test_exp_id, log_output = TRUE)
  Sys.sleep(1)
  sub_pid <- exp$.__enclos_env__$private$logging_process$get_pid()
  expect_equal(nrow(get_subp(sub_pid)), 1)
  exp$stop()
  Sys.sleep(1)
  expect_equal(nrow(get_subp(sub_pid)), 0)
})

test_that("create_experiment fails when the API doesn't return new experiment details", {
  with_mock(
    `cometr:::new_experiment` = function(...) list(), {
      expect_error(create_experiment(), "Create experiment in Comet failed")
    }
  )
})

test_that("create_experiment fails when given wrong parameters", {
  on.exit(cleanup())
  expect_error(mock_experiment_full(keep_active = 5), " must be either TRUE or FALSE")
  expect_error(mock_experiment_full(log_output = 5), " must be either TRUE or FALSE")
  expect_error(mock_experiment_full(log_error = 5), " must be either TRUE or FALSE")
  expect_error(mock_experiment_full(log_code = 5), " must be either TRUE or FALSE")
  expect_error(mock_experiment_full(log_system_details = 5), " must be either TRUE or FALSE")
  expect_error(mock_experiment_full(log_git_info = 5), " must be either TRUE or FALSE")
})

with_mock(
  `cometr:::get_config_logging_file` = function() logfile,
  `cometr:::get_config_logging_file_level` = function() "DEBUG", {

    test_that("create_experiment log_code works", {
      with_mock(
        `cometr:::get_system_script` = function(...) test_path("test-data", "sample-script.R"),
        `cometr:::log_code` = function(...) NULL, {
          on.exit(cleanup())

          mock_experiment_full(log_code = FALSE)
          expect_equal(length(grep("Logging source code", readLines(logfile))), 0)

          cleanup()
          mock_experiment_full(log_code = TRUE)
          expect_equal(length(grep("Logging source code", readLines(logfile))), 1)
        }
      )
    })

    test_that("create_experiment log_system_details works", {
      with_mock(
        `cometr:::log_system_details` = function(...) NULL, {
          on.exit(cleanup())

          mock_experiment_full(log_system_details = FALSE)
          expect_equal(length(grep("Logging system details", readLines(logfile))), 0)

          cleanup()
          mock_experiment_full(log_system_details = TRUE)
          expect_equal(length(grep("Logging system details", readLines(logfile))), 1)
        }
      )
    })

    test_that("create_experiment log_git_info works", {
      with_mock(
        `cometr:::log_system_details` = function(...) NULL, {
          on.exit(cleanup())

          mock_experiment_full(log_system_details = FALSE)
          expect_equal(length(grep("Logging system details", readLines(logfile))), 0)

          cleanup()
          mock_experiment_full(log_system_details = TRUE)
          expect_equal(length(grep("Logging system details", readLines(logfile))), 1)
        }
      )
    })

  }
)

test_that("get_key and get_url and get_metadata work", {
  on.exit(reset_comet_cache())

  exp <- mock_experiment_full(experiment_key = "testkey")
  expect_identical(exp$get_key(), "testkey")
  expect_identical(exp$get_url(), paste0("https://www.comet.com/", test_ws, "/", test_proj, "/", "testkey"))
})

test_that("create and delete work", {
  skip_on_cran()
  skip_if_offline()
  on.exit(reset_comet_cache())

  exp <- create_experiment(experiment_name = test_experiment, project_name = test_proj,
                           api_key = test_api_key, keep_active = FALSE, log_output = FALSE,
                           log_error = FALSE, log_code = FALSE, log_system_details = FALSE,
                           log_git_info = FALSE)
  Sys.sleep(2)

  experiments_post <- get_experiments(project_name = test_proj, api_key = test_api_key)[["experiments"]]
  experiments_post_num <- length(experiments_post)
  expect_true(test_experiment %in% get_values_from_list(experiments_post, "experimentName"))
  exp$delete()
  Sys.sleep(2)

  experiments_end <- get_experiments(project_name = test_proj, api_key = test_api_key)[["experiments"]]
  experiments_end_num <- length(experiments_end)
  expect_equal(experiments_end_num, experiments_post_num - 1)
})

test_that("keepalive process is alive until the experiment stops", {
  skip_if_offline()

  exp <- create_experiment(experiment_name = test_experiment, project_name = test_proj,
                           api_key = test_api_key, keep_active = FALSE, log_output = FALSE,
                           log_error = FALSE, log_code = FALSE, log_system_details = FALSE,
                           log_git_info = FALSE)
  Sys.sleep(1)
  expect_null(exp$.__enclos_env__$private$keepalive_process)
  exp$delete()

  exp <- create_experiment(experiment_name = test_experiment, project_name = test_proj,
                           api_key = test_api_key, keep_active = TRUE, log_output = FALSE,
                           log_error = FALSE, log_code = FALSE, log_system_details = FALSE,
                           log_git_info = FALSE)
  Sys.sleep(1)
  sub_pid <- exp$.__enclos_env__$private$keepalive_process$get_pid()
  expect_equal(nrow(get_subp(sub_pid)), 1)
  exp$delete()
  Sys.sleep(1)
  expect_equal(nrow(get_subp(sub_pid)), 0)
})

