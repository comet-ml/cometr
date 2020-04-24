hasInternet <- function() !is.null(curl::nslookup("r-project.org", error = FALSE))
if (hasInternet()) {

  new_exp_name <- paste0("exp-", generate_random_id())
  test_exp <- create_experiment(experiment_name = new_exp_name, project_name = test_proj,
                                api_key = test_api_key, keep_active = FALSE,
                                log_output = TRUE, log_error = FALSE, log_code = FALSE,
                                log_system_details = FALSE, log_git_info = FALSE)

  test_that("archive and restore work", {
    expect_false(test_exp$get_metadata()$archived)
    test_exp$archive()
    Sys.sleep(2)
    expect_true(test_exp$get_metadata()$archived)
    test_exp$restore()
    Sys.sleep(2)
    expect_false(test_exp$get_metadata()$archived)
  })

  test_that("common user getter/setter functions on Experiment work", {
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

  test_that("set_start_end_time works", {
    start <- epoch_ms()
    end <- epoch_ms() + 20000
    test_exp$set_start_end_time(start = start, end = end)
    Sys.sleep(2)
    expect_equal(start, test_exp$get_metadata()$startTimeMillis)
    expect_equal(start, test_exp$get_metadata()$endTimeMillis)
  })

  test_that("create_symlink works", {
    symlink <- test_exp$create_symlink(project_name = test_proj)
    expect_true(!is.null(symlink$link))
  })

  test_that("automatic getter/setter functions on Experiment work", {
    file <- "sample-script.R"

    test_exp$
      log_git_metadata(branch = "dev", user = "daattali")$
      log_code(readLines(file, warn = FALSE))$
      log_system_details(pid = Sys.getpid())

    Sys.sleep(5)

    git_r <- test_exp$get_git_metadata()
    expect_equal(git_r$branch, "dev")
    expect_equal(git_r$user, "daattali")

    output_r <- test_exp$get_output()$output
    expect_equal(output_r, "")

    code_r <- test_exp$get_code()$code
    expect_equal(code_r, readLines(file, warn = FALSE))

    system_r <- test_exp$get_system_details()
    expect_equal(system_r$pid, Sys.getpid())
  })

  test_exp$delete()
}
