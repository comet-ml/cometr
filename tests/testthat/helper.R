reset_comet_cache <- function() {
  .cometrenv$cache <- list()
  .cometrenv$cache$config <- list()
}

test_api_key <- Sys.getenv("TRAVIS_COMET_API_KEY")
if (test_api_key == "") test_api_key <- get_config_api_key(must_work = TRUE)
reset_comet_cache()
ws <- "cometrtestws"
proj <- "cometrtestproject"
experiment <- "cometrtestexp"
proj_id <- "de6244b389c947699b84e11d82d4b338"
exp_id <- "c169df5d59a1425c9d31743af744efc0"

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
                        log_system_details = log_system_details, log_git_info = log_git_info)
    }
  )
}

get_subp <- function(sub_pid) {
  subset(ps::ps(), pid == sub_pid, ppid = Sys.getpid())
}
