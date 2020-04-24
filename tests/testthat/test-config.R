test_that("config utils work", {
  expect_true(is_config_empty(NULL))
  expect_true(is_config_empty(""))
  expect_false(is_config_empty(5))
  expect_false(is_config_empty("test"))
  expect_false(is_config_empty(FALSE))

  expect_identical(get_config_filename(), .cometrenv$COMET_CONFIG_FILE_NAME)
  expect_identical(R.utils::getAbsolutePath(get_home_dir(), expandTilde = TRUE), R.utils::getAbsolutePath("~", expandTilde = TRUE))
})

with_mock(
  `cometr:::get_config_filename` = function() "comet.yml", {
    test_that("get_config_from_configfile works", {
      on.exit(reset_comet_cache())

      expect_identical(get_config_from_configfile("COMET_WORKSPACE", "config/simple"), "workspace_simple")
      expect_identical(get_config_from_configfile("COMET_PROJECT_NAME", "config/simple"), "project_simple")

      expect_null(get_config_from_configfile("COMET_API_KEY", "config/simple"))

      expect_null(get_config_from_configfile("COMET_WORKSPACE", "config/thisfolderdoesntexist"))

      expect_warning(value <- get_config_from_configfile("COMET_WORKSPACE", "config/error"))
      expect_null(value)
    })

    test_that("retrieving all parameters work", {
      on.exit(reset_comet_cache())
      owd <- setwd("config/full")
      on.exit(setwd(owd), add = TRUE)

      okey <- Sys.getenv("COMET_API_KEY")
      Sys.setenv("COMET_API_KEY" = "")
      on.exit(Sys.setenv("COMET_API_KEY" = okey), add = TRUE)
      oworkspace <- Sys.getenv("COMET_WORKSPACE")
      Sys.setenv("COMET_WORKSPACE" = "")
      on.exit(Sys.setenv("COMET_WORKSPACE" = oworkspace), add = TRUE)
      oproject <- Sys.getenv("COMET_PROJECT_NAME")
      Sys.setenv("COMET_PROJECT_NAME" = "")
      on.exit(Sys.setenv("COMET_PROJECT_NAME" = oproject), add = TRUE)
      ourl <- Sys.getenv("COMET_URL_OVERRIDE")
      Sys.setenv("COMET_URL_OVERRIDE" = "")
      on.exit(Sys.setenv("COMET_URL_OVERRIDE" = ourl), add = TRUE)
      ologfile <- Sys.getenv("COMET_LOGGING_FILE")
      Sys.setenv("COMET_LOGGING_FILE" = "")
      on.exit(Sys.setenv("COMET_LOGGING_FILE" = ologfile), add = TRUE)
      ologlevel <- Sys.getenv("COMET_LOGGING_FILE_LEVEL")
      Sys.setenv("COMET_LOGGING_FILE_LEVEL" = "")
      on.exit(Sys.setenv("COMET_LOGGING_FILE_LEVEL" = ologlevel), add = TRUE)

      expect_identical(get_config_api_key(), "key_full")
      expect_identical(get_config_workspace(), "workspace_full")
      expect_identical(get_config_project_name(), "project_full")
      expect_identical(get_config_url(), "cometrtest.com")
      expect_identical(get_config_logging_file(), R.utils::getAbsolutePath("cometr.log", expandTilde = TRUE))
      expect_identical(get_config_logging_file_level(), "ERROR")
    })

    test_that("retrieving a parameter with must_work works", {
      on.exit(reset_comet_cache())
      owd <- setwd("config/simple")
      on.exit(setwd(owd), add = TRUE)

      okey <- Sys.getenv("COMET_API_KEY")
      Sys.setenv("COMET_API_KEY" = "")
      on.exit(Sys.setenv("COMET_API_KEY" = okey), add = TRUE)
      oworkspace <- Sys.getenv("COMET_WORKSPACE")
      Sys.setenv("COMET_WORKSPACE" = "")
      on.exit(Sys.setenv("COMET_WORKSPACE" = oworkspace), add = TRUE)

      expect_null(get_config_api_key())
      expect_error(get_config_api_key(must_work = TRUE))
      expect_identical(get_config_workspace(), "workspace_simple")
      expect_identical(get_config_workspace(must_work = TRUE), "workspace_simple")
    })

    test_that("retrieving URL uses a default when not provided", {
      on.exit(reset_comet_cache())
      owd <- setwd("config/simple")
      on.exit(setwd(owd), add = TRUE)

      expect_identical(get_config_url(), modify_config_url(.cometrenv$COMET_API_DEFAULT_URL))
    })

    test_that("config priority is correct: envvar -> working dir config file -> home dir config file", {
      config_home_dir <- R.utils::getAbsolutePath("config/homedir", expandTilde = TRUE)
      with_mock(
        `cometr:::get_home_dir` = function() config_home_dir, {
          on.exit(reset_comet_cache())
          owd <- setwd("config/homedir")
          on.exit(setwd(owd), add = TRUE)


          okey <- Sys.getenv("COMET_API_KEY")
          Sys.setenv("COMET_API_KEY" = "")
          on.exit(Sys.setenv("COMET_API_KEY" = okey), add = TRUE)
          oworkspace <- Sys.getenv("COMET_WORKSPACE")
          Sys.setenv("COMET_WORKSPACE" = "")
          on.exit(Sys.setenv("COMET_WORKSPACE" = oworkspace), add = TRUE)
          oproject <- Sys.getenv("COMET_PROJECT_NAME")
          Sys.setenv("COMET_PROJECT_NAME" = "")
          on.exit(Sys.setenv("COMET_PROJECT_NAME" = oproject), add = TRUE)

          expect_identical(get_config_workspace(), "workspace_home")
          expect_identical(get_config_api_key(), "key_home")
          expect_null(get_config_project_name())

          owd <- setwd("../simple")
          reset_comet_cache()
          expect_identical(get_config_workspace(), "workspace_simple")
          expect_identical(get_config_project_name(), "project_simple")
          expect_identical(get_config_api_key(), "key_home")

          reset_comet_cache()
          Sys.setenv("COMET_WORKSPACE" = "workspace_envvar")
          expect_identical(get_config_workspace(), "workspace_envvar")
          expect_identical(get_config_project_name(), "project_simple")
          expect_identical(get_config_api_key(), "key_home")
        }
      )
    })
  }
)
