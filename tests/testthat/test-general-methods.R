httptest::with_mock_api({
  test_that("able to retrieve workspaces", {
    skip_on_cran()
    skip_if_offline()
    on.exit(reset_comet_cache())

    workspaces <- get_workspaces(api_key = test_api_key)[["workspaceNames"]]
    expect_true(test_ws %in% workspaces)
  })

  test_that("able to retrieve projects", {
    skip_on_cran()
    skip_if_offline()
    on.exit(reset_comet_cache())

    projects <- get_projects(workspace_name = test_ws, api_key = test_api_key)[["projects"]]
    project_names <- get_values_from_list(projects, "projectName")
    expect_true(test_proj %in% project_names)
  })

  test_that("able to retrieve experiments", {
    skip_on_cran()
    skip_if_offline()
    on.exit(reset_comet_cache())

    experiments <- get_experiments(project_name = test_proj, workspace_name = test_ws, api_key = test_api_key)[["experiments"]]
    experiment_names <- get_values_from_list(experiments, "experimentName")
    expect_true(test_experiment %in% experiment_names)
  })

  test_that("able to retrieve columns", {
    skip_on_cran()
    skip_if_offline()
    on.exit(reset_comet_cache())

    columns <- get_columns(project_name = test_proj, workspace_name = test_ws, api_key = test_api_key)[["columns"]]
    column_names <- get_values_from_list(columns, "name")
    expect_true(all(c("duration", "experimentKey") %in% column_names))

    columns2 <- get_columns(project_id = test_proj_id, api_key = test_api_key)[["columns"]]
    expect_identical(columns, columns2)
  })

  test_that("getting multi metric chart returns successfully completes an API call", {
    skip_on_cran()
    skip_if_offline()
    on.exit(reset_comet_cache())

    expect_error(get_multi_metric_chart(test_exp_id, api_key = test_api_key), NA)
  })
})

test_that("create a project, verify it exists, delete it", {
  skip_on_cran()
  skip_if_offline()
  on.exit(reset_comet_cache())

  new_proj_name <- paste0("proj-", generate_random_id())

  projects_init <- get_projects(api_key = test_api_key)[["projects"]]
  projects_init_num <- length(projects_init)
  create_project(project_name = new_proj_name, project_description = "description",
                 api_key = test_api_key)

  wait_for("Project created", 20, function(){
    projects_post <- get_projects(api_key = test_api_key)[["projects"]]
    isTRUE(new_proj_name %in% get_values_from_list(projects_post, "projectName"))
  })

  projects_post <- get_projects(api_key = test_api_key)[["projects"]]
  expect_true(new_proj_name %in% get_values_from_list(projects_post, "projectName"))
  # delete and check that it was actually deleted
  delete_project(project_name = new_proj_name, api_key = test_api_key)

  wait_for("Project deleted", 20, function(){
    projects_end <- get_projects(api_key = test_api_key)[["projects"]]
    isFALSE(new_proj_name %in% get_values_from_list(projects_end, "projectName"))
  })
  projects_end <- get_projects(api_key = test_api_key)[["projects"]]
  expect_false(new_proj_name %in% get_values_from_list(projects_end, "projectName"))
})
