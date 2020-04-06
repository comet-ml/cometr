if (!is.null(curl::nslookup("r-project.org", error = FALSE))) {
  new_exp_name <- paste0("exp-", generate_random_id())
  test_exp <- create_experiment(experiment_name = new_exp_name, project_name = proj,
                                workspace_name = ws, api_key = test_api_key, keep_active = FALSE,
                                log_output = FALSE, log_error = FALSE, log_code = FALSE,
                                log_system_details = FALSE, log_git_info = FALSE)
}

test_that("archive and restore work", {
  skip_on_cran()
  skip_if_offline()

  expect_false(test_exp$get_metadata()$archived)
  test_exp$archive()
  Sys.sleep(2)
  expect_true(test_exp$get_metadata()$archived)
  test_exp$restore()
  Sys.sleep(2)
  expect_false(test_exp$get_metadata()$archived)
})

test_that("getter/setter functions on Experiment work", {
  skip_on_cran()
  skip_if_offline()

  metric <- 5
  graph <- jsonlite::toJSON(list(nodes = list("a","b","c"), edges = list("a,b", "c,a")))
  param <- 10
  other <- "some value"
  tags <- c("t1", "t2")
  html <- "<div style='color: red'>test</div>"

  test_exp$
    log_metric("metric1", metric)$
    log_graph(graph)$
    log_parameter("param1", param)$
    log_other("other1", other)$
    add_tags(tags)$
    log_html(html, override = FALSE)

  Sys.sleep(5)

  # get_metric("metric1")
  # get_metrics_summary
  # get_graph
  # get_parameters
  # get_other
  # get_tags
  # get_html
})

test_exp$delete()
