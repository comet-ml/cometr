test_api_key <- Sys.getenv("TRAVIS_COMET_API_KEY")
if (test_api_key == "") test_api_key <- get_config_api_key(must_work = TRUE)
ws <- "cometrtestws"
proj <- "cometrtestproject"
proj_id <- "de6244b389c947699b84e11d82d4b338"
exp_id <- "c169df5d59a1425c9d31743af744efc0"

mock_experiment_by_id <- function(experiment_key, keep_active = FALSE,
                                  log_output = FALSE, log_error = FALSE) {
  .cometrenv$cancreate <- TRUE
  Experiment$new(experiment_key = experiment_key, api_key = test_api_key,
                 keep_active = keep_active, log_output = log_output, log_error = log_error)
}

mock_experiment_full <- function(experiment_key = generate_random_id(), keep_active = FALSE,
                                 log_output = FALSE, log_error = FALSE, log_code = FALSE,
                                 log_system_details = FALSE, log_git_info = FALSE) {
  link <- paste0("https://www.comet.ml/", ws, "/", proj, "/", experiment_key)
  with_mock(
    `cometr:::new_experiment` = function(...) list(experimentKey = experiment_key, link = link), {
      create_experiment(api_key = test_api_key, keep_active = keep_active, log_output = log_output,
                        log_error = log_error, log_code = log_code,
                        log_system_details= log_system_details, log_git_info = log_git_info)
    }
  )
}

get_subp <- function(sub_pid) {
  subset(ps::ps(), pid == sub_pid, ppid = Sys.getpid())
}

### Tests begin below


test_that("cannot create an Experiment object directly", {
  expect_error(Experiment$new(), "Do not call this function directly.")
})

test_that("creating a second experiment causes the first to stop", {
  skip_on_cran()
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
  skip_on_cran()
  on.exit(reset_comet_cache())

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

test_that("keepalive process is alive until the experiment stops", {
  skip_if_offline()

  exp <- mock_experiment_full(experiment_key = exp_id, keep_active = FALSE)
  Sys.sleep(1)
  expect_null(exp$.__enclos_env__$private$keepalive_process)
  exp$stop()

  exp <- mock_experiment_full(experiment_key = exp_id, keep_active = TRUE)
  Sys.sleep(1)
  sub_pid <- exp$.__enclos_env__$private$keepalive_process$get_pid()
  expect_equal(nrow(get_subp(sub_pid)), 1)
  exp$stop()
  Sys.sleep(1)
  expect_equal(nrow(get_subp(sub_pid)), 0)
})

test_that("logging process is alive until the experiment stops", {
  exp <- mock_experiment_full(experiment_key = exp_id, log_output = FALSE)
  Sys.sleep(1)
  expect_null(exp$.__enclos_env__$private$logging_process)
  exp$stop()

  exp <- mock_experiment_full(experiment_key = exp_id, log_output = TRUE)
  Sys.sleep(1)
  sub_pid <- exp$.__enclos_env__$private$logging_process$get_pid()
  expect_equal(nrow(get_subp(sub_pid)), 1)
  exp$stop()
  Sys.sleep(1)
  expect_equal(nrow(get_subp(sub_pid)), 0)
})

test_that("create experiment fails when the API doesn't return new experiment details", {
  with_mock(
    `cometr:::new_experiment` = function(...) list(), {
      expect_error(create_experiment(), "Create experiment in Comet failed")
    }
  )
})

test_that("create experiment fails with wrong parameters", {
  expect_error(mock_experiment_full(keep_active = 5), " must be either TRUE or FALSE")
  expect_error(mock_experiment_full(log_output = 5), " must be either TRUE or FALSE")
  expect_error(mock_experiment_full(log_error = 5), " must be either TRUE or FALSE")
  expect_error(mock_experiment_full(log_code = 5), " must be either TRUE or FALSE")
  expect_error(mock_experiment_full(log_system_details = 5), " must be either TRUE or FALSE")
  expect_error(mock_experiment_full(log_git_info = 5), " must be either TRUE or FALSE")
})

logfile <- R.utils::getAbsolutePath("cometr.log")
cleanup <- function() {
  if (file.exists(logfile)) {
    file.remove(logfile)
  }
  reset_comet_cache()
}

with_mock(
  `cometr:::get_config_logging_file` = function() logfile,
  `cometr:::get_config_logging_file_level` = function() "DEBUG", {

    test_that("create experiment log code works", {
      with_mock(
        `cometr:::get_system_script` = function(...) "sample-script.R",
        `cometr:::log_code` = function(...) NULL, {
          on.exit(cleanup())

          cleanup()
          mock_experiment_full(log_code = FALSE)
          expect_equal(length(grep("Logging source code", readLines(logfile))), 0)

          cleanup()
          mock_experiment_full(log_code = TRUE)
          expect_equal(length(grep("Logging source code", readLines(logfile))), 1)
        }
      )
    })

    test_that("create experiment log system details works", {
      with_mock(
        `cometr:::log_system_details` = function(...) NULL, {
          on.exit(cleanup())

          cleanup()
          mock_experiment_full(log_system_details = FALSE)
          expect_equal(length(grep("Logging system details", readLines(logfile))), 0)

          cleanup()
          mock_experiment_full(log_system_details = TRUE)
          expect_equal(length(grep("Logging system details", readLines(logfile))), 1)
        }
      )
    })

    test_that("create experiment log git info works", {
      with_mock(
        `cometr:::log_system_details` = function(...) NULL, {
          on.exit(cleanup())

          cleanup()
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
