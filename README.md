# cometr - Comet API for R

[![CRAN version](https://www.r-pkg.org/badges/version/cometr)](https://cran.r-project.org/package=cometr)
[![R](https://github.com/comet-ml/cometr/actions/workflows/r.yml/badge.svg?branch=master)](https://github.com/comet-ml/cometr/actions/workflows/r.yml)

[Comet](https://www.comet.com/) is a cloud platform allowing you to track, compare, explain and optimize machine learning experiments and models. If you perform any machine learning in R, you can use the `cometr` R package to create and track Comet experiments. All experiment data is available for retrieval through `cometr` or can be viewed on the [Comet dashboard](https://www.comet.com/).

You can read more about Comet to see its full capabilities on [Comet's website](https://www.comet.com/). The package currently supports Comet's REST API v2.

## Installation

To install the stable CRAN version:

```R
install.packages("cometr")
```

To install the latest development version from GitHub:

```R
install.packages("remotes")
remotes::install_github("comet-ml/cometr")
```

## One-time setup

To use `cometr`, you must first have a [Comet](https://www.comet.com/) account (you can sign up for a free account) and obtain your personal API key from the 'Settings' page on Comet's website. The API key can either be passed into every `cometr` function call as an argument, or it can be defined as a configuration variable as described later in the 'Configuration variables' section.

## How to use

`cometr` is most useful when used in an R script that is run using the `Rscript` tool. It can also be used in interactive environments (such as the R shell or RStudio). To use Comet in your R script, you need to first load `cometr` and create a Comet Experiment by calling `create_experiment()`.

```R
library(cometr)
API_KEY <- "<your API key>"
exp <- create_experiment(experiment_name = "my-first-experiment",
                         project_name = "project1",
                         workspace_name = "daattali",
                         api_key = API_KEY)
```

You'll see a message displayed with a URL where you can view this newly created Experiment online.

```
Experiment created: https://www.comet.com/daattali/project1/46d53bbce4e14fb692585b6e73da6e0c
```

By default, creating an experiment will log your system's details to Comet's servers, keep track of how long your experiment is running for, log any output that gets generated, and perform a few more house-keeping tasks automatically. You can run `?cometr::create_experiment` to see all the supported features and turn them on or off. 

Now that an Experiment object has been created, you can run your machine learning code as usual. The `exp` variable you created is an Experiment object (which is an R6 object--if you don't know what that means then don't worry about it!), and we can use it to set parameters of the experiment and to retrieve information from it. To see all the methods that are supported on Experiment objects, run `?cometr::Experiment`.

Here is an example of how you can use the `exp` object to set or retrieve experiment information:

```R
> exp$get_key()
[1] "46d53bbce4e14fb692585b6e73da6e0c"
> exp$log_metric("metric1", 5, step = 1)
> exp$log_metric("metric1", 10, step = 2)
> exp$log_graph('{"nodes":[["a"],["b"],["c"]],"edges":[["a,b"],["c,a"]]}')
> str(exp$get_metrics_summary())
List of 1
 $ values:List of 1
  ..$ :List of 10
  .. ..$ name            : chr "metric1"
  .. ..$ valueMax        : chr "10.0"
  .. ..$ valueMin        : chr "5.0"
  .. ..$ valueCurrent    : chr "10.0"
  .. ..$ timestampMax    : num 1.59e+12
  .. ..$ timestampMin    : num 1.59e+12
  .. ..$ timestampCurrent: num 1.59e+12
  .. ..$ stepMax         : int 2
  .. ..$ stepMin         : int 1
  .. ..$ stepCurrent     : int 2
> exp$get_graph()
$graph
[1] "{\"nodes\":[[\"a\"],[\"b\"],[\"c\"]],\"edges\":[[\"a,b\"],[\"c,a\"]]}"
```

A Comet Experiment will automatically end when the R script that created it finishes running. However, it's recommended to explicitly stop the experiment with `exp$stop()`.

While the Experiment object and its associated functions are the cornerstone of `cometr`, there are other functions that aren't directly related to a specific experiment, such as `get_workspaces()`, `get_projects()`, `get_experiments()`, `create_project()`, `delete_project()`.

```R
> get_workspaces(api_key = API_KEY)
$workspaceNames
$workspaceNames[[1]]
[1] "cometrtestws"

$workspaceNames[[2]]
[1] "daattali"
```

### Comet Artifacts Support

Comet Artifacts allow you to keep track of any data associated with the ML Lifecycle. Depending on your application you might decide to either upload the dataset directly to Comet or using the Remote Artifacts feature to store a reference to it instead. No matter the option you choose, Comet will maintain the lineage between your datasets and the training runs that created or consumed them.

Artifacts live at a Comet Workspace level and are identified by their name. Each Artifact can have multiple versions allowing you to keep track of exactly which dataset was used.

#### Logging an Artifact

Logging an Artifact has three steps:

* Create an Artifact Version
* Add files and folders to this Artifact Version
* Logging this Artifact Version to an Experiment

**Log an Artifact to Comet**

When you are ready to send the Artifact to Comet, you will log it with `Experiment$log_artifact(ARTIFACT)`.

Here is an example of how you can use the `exp` object to log Comet artifact:

```R
metadata <- list(foo="bar")
tags <- list("tag1", "tag2")
aliases <- list("alias1", "alias2")
remote_uri <- "http://localhost/dataset.dat"
artifact <- create_artifact(artifact_name = "artifact-name",
                            artifact_type = "dataset",
                            metadata = metadata,
                            aliases = aliases,
                            version_tags = tags)

# Add remote asset to the Artifact                            
artifact$add_remote(uri = remote_uri,
                   logical_path = "dataset",
                   metadata = metadata)

# Add local folder to the Artifact
artifact$add(local_path = "./data-folder",
             logical_path = "data-files",
             metadata = metadata)

# Add single file to the Artifact
artifact$add(local_path = "./sample-script.R",
            logical_path = "samle-code",
            metadata = metadata)
             
exp$log_artifact(artifact)
```

The code above will create new Comet Artifact. After that remote and local assets will be added to it and Artifact with all assets will be uploaded to Comet.

**Access a logged Artifact version**

You can retrieve a logged Artifact from any workspace that you have permission to access, and a workspace name with the `Experiment$get_artifact()` method:

```R
logged_artifact <- exp$get_artifact(artifact_name=NAME,
                                    workspace=WORKSPACE,
                                    version_or_alias=VERSION_OR_ALIAS)
```

To make it easier to access the artifact, you can retrieve a logged Artifact in three ways:

* Get the latest Artifact version by leaving out the `version_or_alias` arguments.
* Get a specific Artifact version by passing in the `version_or_alias` argument.
* Get an aliased Artifact version by passing in the `version_or_alias` argument.
* Get a specific Artifact version by passing in the `artifact_name` in the form of full name of the Artifact and leaving out the other arguments.

The full name of the Artifact has the following format `workspace/artifact-name:version_or_alias` and can take the following forms as an example:

* `workspace\artifact-name:2.0.0` - will get the `2.0.0` version of the artifact `artifact-name` from workspace `workspace`
* `artifact-name:2.0.0` - will get the `2.0.0` version of the artifact `artifact-name` from default user's workspace
* `artifact-name` - will get the latest version of the artifact `artifact-name` from default user's workspace

You can also use an alias for a specific version of the Artifact in the above examples instead of the semantic version if you prefer.

#### Download an Artifact

The process of using an Artifact is similar to the one used to create it.

Here is an example you can use `logged_artifact` to download its content into local folder:

```R
artifact <- logged_artifact$download(path = "./data", overwrite_strategy = FALSE)
```

Where `overwrite_strategy` defines how existing files should be handled:

* `FALSE` or "FAIL": If a file already exists and its content is different, raise the error.
* "PRESERVE": If a file already exists and its content is different, show a WARNING but preserve the existing content.
* `TRUE` or "OVERWRITE": If a file already exists and its content is different, replace it by the asset version asset.

**Learn more**

You can find more details about Comet Artifacts at:

* [Use Artifacts](https://www.comet.com/docs/v2/guides/data-management/using-artifacts/): an overview of the Comet Artifacts feature.
* [Artifacts UI](https://www.comet.com/docs/v2/guides/comet-dashboard/artifacts/): Artifacts pages in the Comet UI.

## Configuration variables

Since it can get tedious to repeat the `api_key` in different function calls, and it can also be a security concern to include the API key in your code, you can set the API key and a few other variables as `cometr` configuration variables.

The following table lists all the configuration variables that `cometr` uses. All of these variables are optional.

| Variable | Description |
|---|---|
| `COMET_API_KEY` | Personal Comet API key to use in every function that accepts an `api_key` parameter. \*\*\* |
| `COMET_WORKSPACE` | Default workspace to use in every function that accepts a `workspace_name` parameter. \*\*\* |
| `COMET_PROJECT_NAME` | Default project to use in every function that accepts a `project_name` parameter. \*\*\* |
| `COMET_LOGGING_FILE` | File to store detailed logs produced by `cometr`. This is mostly useful for troubleshooting. |
| `COMET_LOGGING_FILE_LEVEL` | If a logging file is provided, set the level of information to log. Must be one of "DEBUG", "INFO", "ERROR". |
| `COMET_URL_OVERRIDE` | Custom Comet API URL, if you have one. |

_\*\*\* If this value is not provided as a configuration variable, it must be provided as an argument to the relevant functions. If a value is provided as an argument directly to the function, that argument takes precedence and the configuration value is ignored._

Configuration variables can be set either as environment variables or in a `cometr` config file.

A `cometr` config file is a YAML file named `.comet.yml` that can be defined either in the current working directory or in the user's home directory. For example, a `cometr` config file that defines a default workspace and a default project could look like this:

```
COMET_WORKSPACE: my_workspace
COMET_PROJECT_NAME: my_project
```

If a configuration variable is defined in multiple places, then the order of precedence is as follows:

1. Function argument, if relevant (for API keys, workspace name, project name)
2. Environment variable
3. `.comet.yml` config file in the local working directory
4. `.comet.yml` config file in the home directory
