test_that("able to retrieve workspaces and experiments", {
  skip_on_cran()
  skip_if_offline()
  on.exit(reset_comet_cache())

  api_key <- Sys.getenv("TRAVIS_COMET_API_KEY")
  if (api_key == "") api_key <- NULL
  ws <- "cometrtestws"
  proj <- "cometrtestproject"

  workspaces <- get_workspaces(api_key = api_key)[["workspaceNames"]]
  expect_true(all(c("daattali", "cometrtestws") %in% workspaces))

  experiments <- get_experiments(project_name = proj, workspace_name = ws, api_key = api_key)[["experiments"]]
  experiment_names <- get_values_from_list(experiments, "experimentName")
  expect_true(all(c("testexperiment1", "testexperiment2") %in% experiment_names))
})

test_that("create a project, verify it exists, delete it", {
  skip_on_cran()
  skip_if_offline()
  on.exit(reset_comet_cache())

  api_key <- Sys.getenv("TRAVIS_COMET_API_KEY")
  if (api_key == "") api_key <- NULL
  ws <- "cometrtestws"
  new_proj_name <- paste0("proj-", generate_random_id())

  projects_init <- get_projects(workspace_name = ws, api_key = api_key)[["projects"]]
  projects_init_num <- length(projects_init)
  create_project(project_name = new_proj_name, project_description = "description",
                 workspace_name = ws, api_key = api_key)
  Sys.sleep(3)

  projects_post <- get_projects(workspace_name = ws, api_key = api_key)[["projects"]]
  projects_post_num <- length(projects_post)
  expect_equal(projects_post_num, projects_init_num + 1)
  expect_true(new_proj_name %in% get_values_from_list(projects_post, "projectName"))
  delete_project(project_name = new_proj_name, workspace_name = ws, api_key = api_key)
  Sys.sleep(3)

  projects_end <- get_projects(workspace_name = ws, api_key = api_key)[["projects"]]
  projects_end_num <- length(projects_end)
  expect_equal(projects_end_num, projects_init_num)
  expect_false(new_proj_name %in% get_values_from_list(projects_end, "projectName"))
})

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
  expect_identical(exp$get_metric("metric1")[["metrics"]][[1]][["metricValue"]], "6")
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
