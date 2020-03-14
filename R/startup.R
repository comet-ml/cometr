# Get the full path of the script that was called by the shell,
# or "" if it cannot be determined
get_startup_script <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_idx <- grep("^--file=", args)
  filename <- ""
  if (length(file_idx) == 1) {
    filename <- sub("^--file=", "", args[file_idx])
    filename <- R.utils::getAbsolutePath(filename, expandTilde = TRUE)
  }
  filename
}

# Get the command-line arguments that were used to run this script
get_startup_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  args <- paste(args, collapse = " ")
  args
}

get_startup_packages <- function() {
  packages <- installed.packages(noCache = TRUE, priority = "NA")
  packages <- as.data.frame(packages, stringsAsFactors = FALSE)
  rownames(packages) <- NULL
  packages <- packages[, c("Package", "Version")]
  packages
}

get_startup_executable <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  if (length(args) >= 1) {
    args[1]
  } else {
    ""
  }
}

get_startup_hostname <- function() {
  hostname <- R.utils::getHostname.System()
  if (length(hostname) >= 1) {
    hostname[[1]]
  } else {
    ""
  }
}

get_startup_pid <- function() {
  Sys.getpid()
}

get_startup_rVersion <- function() {
  as.character(getRversion())
}

get_startup_rVersionVerbose <- function() {
  R.version.string
}

get_startup_user <- function() {
  user <- R.utils::getUsername.System()
  if (length(user) >= 1) {
    user[[1]]
  } else {
    ""
  }
}
