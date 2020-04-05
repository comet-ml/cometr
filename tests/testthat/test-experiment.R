test_that("create an experiment, add tags and HTML and a metric, retrieve them and the output", {
  skip_on_cran()
  skip_if_offline()
  on.exit(reset_comet_cache())

  api_key <- Sys.getenv("TRAVIS_COMET_API_KEY")
  if (api_key == "") api_key <- NULL
  ws <- "cometrtestws"
  proj <- "cometrtestproject"
  new_exp_name <- paste0("exp-", generate_random_id())

  tags <- c("tag1", "tag2")
  html <- "<em>italics</em>"

  exp <- create_experiment(
    experiment_name = new_exp_name,
    project_name = proj,
    workspace_name = ws,
    api_key = api_key,
    keep_active = FALSE,
    log_output = TRUE,
    log_error = FALSE,
    log_code = FALSE,
    log_system_details = FALSE,
    log_git_info = FALSE
  )

  exp$add_tags(tags)
  exp$log_html(html)
  exp$log_metric("metric1", 5)

  Sys.sleep(3)

  cat(exp$get_html()[["html"]])
  exp$stop()

  Sys.sleep(3)

  expect_identical(exp$get_metadata()[["experimentName"]], new_exp_name)
  expect_identical(exp$get_tags()[["tags"]], as.list(tags))
  expect_identical(exp$get_html()[["html"]], html)
  expect_identical(exp$get_metric("metric1")[["metrics"]][[1]][["metricValue"]], "5")
  expect_identical(exp$get_output()[["output"]], html)

  exp$delete()
})

test_that("creating a second experiment causes the first to stop and unable to modify the first", {
  skip_on_cran()
  skip_if_offline()
  on.exit(reset_comet_cache())

  api_key <- Sys.getenv("TRAVIS_COMET_API_KEY")
  if (api_key == "") api_key <- NULL
  ws <- "cometrtestws"
  proj <- "cometrtestproject"

  exp1 <- create_experiment(project_name = proj, workspace_name = ws, api_key = api_key,
                            log_output = FALSE, log_error = FALSE, log_code = FALSE,
                            keep_active = FALSE, log_system_details = FALSE)
  Sys.sleep(3)
  expect_output(
    exp2 <- create_experiment(project_name = proj, workspace_name = ws, api_key = api_key,
                              log_output = FALSE, log_error = FALSE, log_code = FALSE,
                              keep_active = FALSE, log_system_details = FALSE),
    "will be stopped"
  )
  expect_error(exp1$add_tags("tag1"), "already ended")

  exp1$delete()
  exp2$delete()
})
