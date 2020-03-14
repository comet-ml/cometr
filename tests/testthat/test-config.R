test_that("config utils work", {
  expect_true(is_config_empty(NULL))
  expect_true(is_config_empty(""))
  expect_false(is_config_empty(5))
  expect_false(is_config_empty("test"))
  expect_false(is_config_empty(FALSE))

  expect_identical(normalizePath(get_home_dir()), normalizePath("~"))
})

with_mock(
  `cometr:::get_config_filename` = function() "comet.yml",
  {
    test_that("get_config_from_configfile works", {
      expect_identical(get_config_from_configfile("COMET_WORKSPACE", "config/simple"), "workspace_simple")
      expect_identical(get_config_from_configfile("COMET_PROJECT_NAME", "config/simple"), "project_simple")

      expect_null(get_config_from_configfile("COMET_API_KEY", "config/simple"))

      expect_null(get_config_from_configfile("COMET_WORKSPACE", "config/thisfolderdoesntexist"))

      expect_warning(value <- get_config_from_configfile("COMET_WORKSPACE", "config/error"))
      expect_null(value)
    })

    test_that("retrieving all parameters work", {
      owd <- setwd("config/full")
      on.exit(setwd(owd))
      reset_comet_cache()
      expect_identical(get_config_api_key(), "key_full")
      expect_identical(get_config_workspace(), "workspace_full")
      expect_identical(get_config_project_name(), "project_full")
      expect_identical(get_config_url(), "comet.ml")
      message("FILEEEEEEEEEEEEE:")
      message(get_config_logging_file())
      expect_identical(get_config_logging_file(), normalizePath("cometr.log", mustWork = FALSE))
      expect_identical(get_config_logging_file_level(), "ERROR")
    })

    test_that("retrieving a parameter with must_work works", {
      owd <- setwd("config/simple")
      on.exit(setwd(owd))
      reset_comet_cache()
      expect_null(get_config_api_key())
      expect_error(get_config_api_key(must_work = TRUE))
      expect_identical(get_config_workspace(), "workspace_simple")
      expect_identical(get_config_workspace(must_work = TRUE), "workspace_simple")
    })

    test_that("retrieving URL uses a default when not provided", {
      owd <- setwd("config/simple")
      on.exit(setwd(owd))
      reset_comet_cache()
      expect_identical(get_config_url(), .cometenv$COMET_API_DEFAULT_URL)
    })

    test_that("config priority is correct: envvar -> working dir config file -> home dir config file", {
      config_home_dir <- normalizePath("config/homedir")
      with_mock(
        `cometr:::get_home_dir` = function() config_home_dir,
        {
          owd <- setwd("config/homedir")
          on.exit(setwd(owd))
          reset_comet_cache()
          expect_identical(get_config_workspace(), "workspace_home")
          expect_identical(get_config_api_key(), "key_home")
          expect_null(get_config_project_name())

          owd <- setwd("../simple")
          reset_comet_cache()
          expect_identical(get_config_workspace(), "workspace_simple")
          expect_identical(get_config_project_name(), "project_simple")
          expect_identical(get_config_api_key(), "key_home")

          reset_comet_cache()
          oworkspace <- Sys.getenv("COMET_WORKSPACE")
          Sys.setenv("COMET_WORKSPACE" = "workspace_envvar")
          on.exit(Sys.setenv("COMET_WORKSPACE" = oworkspace))
          expect_identical(get_config_workspace(), "workspace_envvar")
          expect_identical(get_config_project_name(), "project_simple")
          expect_identical(get_config_api_key(), "key_home")
        }
      )
    })
  }
)
