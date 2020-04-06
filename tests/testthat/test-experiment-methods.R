hasInternet <- function() !is.null(curl::nslookup("r-project.org", error = FALSE))

if (!hasInternet()) return()

new_exp_name <- paste0("exp-", generate_random_id())
test_exp <- create_experiment(experiment_name = new_exp_name, project_name = proj,
                              workspace_name = ws, api_key = test_api_key, keep_active = FALSE,
                              log_output = FALSE, log_error = FALSE, log_code = FALSE,
                              log_system_details = FALSE, log_git_info = FALSE)

test_that("archive and restore work", {
  skip_on_cran()

  expect_false(test_exp$get_metadata()$archived)
  test_exp$archive()
  Sys.sleep(2)
  expect_true(test_exp$get_metadata()$archived)
  test_exp$restore()
  Sys.sleep(2)
  expect_false(test_exp$get_metadata()$archived)
})

test_that("common user getter/setter functions on Experiment work", {
  skip_on_cran()

  metric <- 5
  graph <- jsonlite::toJSON(list(nodes = list("a", "b", "c"), edges = list("a,b", "c,a")))
  param <- 10
  other <- "some value"
  tags <- c("t1", "t2")
  html <- "<div style='color: red'>test</div>"
  file <- "sample-script.R"

  test_exp$
    log_metric("metric1", metric)$
    log_graph(graph)$
    log_parameter("param1", param)$
    log_other("other1", other)$
    add_tags(tags)$
    log_html(html, override = FALSE)$
    upload_asset(file)

  Sys.sleep(5)

  metric_r <- test_exp$get_metric("metric1")$metrics
  expect_equal(as.numeric(metric_r[[1]]$metricValue), metric)

  metrics_r <- test_exp$get_metrics_summary()$values
  expect_equal(metrics_r[[1]]$name, "metric1")
  expect_equal(as.numeric(metrics_r[[1]]$valueCurrent), metric)

  graph_r <- test_exp$get_graph()$graph
  expect_equal(graph_r, as.character(graph))

  params_r <- test_exp$get_parameters()$values
  expect_equal(params_r[[1]]$name, "param1")
  expect_equal(as.numeric(params_r[[1]]$valueCurrent), param)

  other_r <- test_exp$get_other()$values
  other_r <- Filter(function(x) x$name == "other1", other_r)
  expect_equal(other_r[[1]]$valueCurrent, other)

  tags_r <- test_exp$get_tags()$tags
  expect_true(all(tags %in% unlist(tags_r)))

  html_r <- test_exp$get_html()$html
  expect_equal(html_r, html)

  assets <- test_exp$get_asset_list()$assets
  expect_equal(assets[[1]]$fileName, file)
  asset_r <- test_exp$get_asset(assetId = assets[[1]]$assetId)
  expect_equal(asset_r, readLines(file, warn = FALSE))
})

test_that("create_symlink works", {
  symlink <- test_exp$create_symlink(project_name = "cometrtestproject2")
  expect_true(!is.null(symlink$link))
})

test_exp$delete()
