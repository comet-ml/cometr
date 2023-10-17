reset_comet_cache <- function() {
  .cometrenv$cache <- list()
  .cometrenv$cache$config <- list()
}

test_api_key <- get_config_api_key(must_work = TRUE)
reset_comet_cache()
test_proj <- "cometrtestproject"
test_ws <- "testuser-cometr"
test_experiment <- "testexperiment1"
test_proj_id <- "ebfbbc1212004f92b618d50db41f27f3"
test_exp_id <- "c352827bcdcb45e3ad40d0decec0b43c"

logfile <- "cometr.log"
cleanup <- function() {
  if (file.exists(logfile)) {
    file.remove(logfile)
  }
  reset_comet_cache()
}

mock_experiment_by_id <- function(experiment_key, keep_active = FALSE,
                                  log_output = FALSE, log_error = FALSE) {
  .cometrenv$cancreate <- TRUE
  with_mock(
    `cometr:::experiment_log_metadata` = function(...) {
    },
    Experiment$new(
      experiment_key = experiment_key,
      api_key = test_api_key,
      keep_active = keep_active,
      log_output = log_output,
      log_error = log_error
    )
  )
}

mock_experiment_full <- function(experiment_key = generate_random_id(), keep_active = FALSE,
                                 log_output = FALSE, log_error = FALSE, log_code = FALSE,
                                 log_system_details = FALSE, log_git_info = FALSE) {
  link <- paste0("https://www.comet.com/", test_ws, "/", test_proj, "/", experiment_key)
  with_mock(
    `cometr:::new_experiment` = function(...)
      list(experimentKey = experiment_key, link = link),
    {
      with_mock(
        `cometr:::experiment_log_metadata` = function(...) {
        },
        create_experiment(
          api_key = test_api_key,
          keep_active = keep_active,
          log_output = log_output,
          log_error = log_error,
          log_code = log_code,
          log_system_details = log_system_details,
          log_git_info = log_git_info
        )
      )
    }
  )
}

get_subp <- function(sub_pid) {
  subset(ps::ps(), pid == sub_pid, ppid == Sys.getpid())
}

hasInternet <- function() !is.null(curl::nslookup("r-project.org", error = FALSE))

asset_by_name <- function(assets, logical_path) {
  selection <- sapply(assets, function(f)
    f$get_logical_path() == logical_path)
  assets[selection]
}

wait_for <- function(reason, timeout, callback) {
  start_time <- Sys.time()
  elapsed <-  0
  while (!callback()) {
    now <- Sys.time()
    elapsed <- now - start_time
    remaining <- timeout - elapsed
    if (remaining < 0) {
      comet_stop(paste0("waited too long (>", timeout, " seconds) for: '", reason, "'"))
    }
    LOG_INFO(sprintf("waiting for: '%s' (%03.0fs left)", reason, remaining),
             echo = TRUE)
    Sys.sleep(1)
  }
  LOG_INFO(sprintf(
    "finished waiting for '%s', it took %03.0f seconds",
    reason,
    elapsed
  ),
  echo = TRUE)
}
