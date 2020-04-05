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

test_that("creating a second experiment causes the first to stop", {
  skip_on_cran()
  skip_if_offline()
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
  skip_if_offline()
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


test_that("create an experiment, add tags and HTML and a metric, retrieve them and the output", {
  # skip_on_cran()
  # skip_if_offline()
  # on.exit(reset_comet_cache())
  #
  # new_exp_name <- paste0("exp-", generate_random_id())
  #
  # tags <- c("tag1", "tag2")
  # html <- "<em>italics</em>"
  #
  # exp <- create_experiment(
  #   experiment_name = new_exp_name,
  #   project_name = proj,
  #   workspace_name = ws,
  #   api_key = api_key,
  #   keep_active = FALSE,
  #   log_output = FALSE,
  #   log_error = FALSE,
  #   log_code = FALSE,  # skip_on_cran()
  # skip_if_offline()
  # on.exit(reset_comet_cache())
  #
  # new_exp_name <- paste0("exp-", generate_random_id())
  #
  # tags <- c("tag1", "tag2")
  # html <- "<em>italics</em>"
  #
  # exp <- create_experiment(
  #   experiment_name = new_exp_name,
  #   project_name = proj,
  #   workspace_name = ws,
  #   api_key = api_key,
  #   keep_active = FALSE,
  #   log_output = FALSE,
  #   log_error = FALSE,
  #   log_code = FALSE,
  #   log_system_details = FALSE,
  #   log_git_info = FALSE
  # )
  #
  # exp$add_tags(tags)
  # exp$log_html(html)
  # exp$log_metric("metric1", 5)
  #
  # Sys.sleep(3)
  #
  # cat(exp$get_html()[["html"]])
  # exp$stop()
  #
  # Sys.sleep(3)
  #
  # expect_identical(exp$get_metadata()[["experimentName"]], new_exp_name)
  # expect_identical(exp$get_tags()[["tags"]], as.list(tags))
  # expect_identical(exp$get_html()[["html"]], html)
  # expect_identical(exp$get_metric("metric1")[["metrics"]][[1]][["metricValue"]], "5")
  # expect_identical(exp$get_output()[["output"]], html)
  #
  # exp$delete()
  #   log_system_details = FALSE,
  #   log_git_info = FALSE
  # )
  #
  # exp$add_tags(tags)
  # exp$log_html(html)
  # exp$log_metric("metric1", 5)
  #
  # Sys.sleep(3)
  #
  # cat(exp$get_html()[["html"]])
  # exp$stop()
  #
  # Sys.sleep(3)
  #
  # expect_identical(exp$get_metadata()[["experimentName"]], new_exp_name)
  # expect_identical(exp$get_tags()[["tags"]], as.list(tags))
  # expect_identical(exp$get_html()[["html"]], html)
  # expect_identical(exp$get_metric("metric1")[["metrics"]][[1]][["metricValue"]], "5")
  # expect_identical(exp$get_output()[["output"]], html)
  #
  # exp$delete()
})


