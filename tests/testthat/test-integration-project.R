test_that("create a project, verify it exists, delete it", {
  skip_on_cran()
  skip_if_offline()

  api_key <- Sys.getenv("TRAVIS_COMET_API_KEY")
  if (api_key == "") api_key <- NULL
  ws <- "cometrtestws"

  projects_init <- get_projects(workspace_name = ws, api_key = api_key)[["projects"]]
  projects_init_num <- length(projects_init)
  new_proj_name <- paste0("proj-", epoch_ms(), "-",
                          paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ""))
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
