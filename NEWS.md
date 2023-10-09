# cometr 0.3.0

* Added support for logging Comet Artifacts `Experiment$log_artifact(artifact)`
* Added experiment method to get workspace name `get_workspace_name()`
* Added experiment method to get project name `get_project_name()`

# cometr 0.2.0

* Added `get_experiment("PREVIOUS-EXPERIMENT-KEY")`
    * Backwards compatible with previous versions
    * Continue training with previous experiments
    * Create multiple experiments for parallel code
* All experiments now `log_other("Created from", "cometr")`

# cometr 0.1.1

* Updated authors, maintainers in DESCRIPTION
* Added example code in caret, keras, and nnet

# cometr 0.1.0

* Initial version
