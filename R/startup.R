# Get the full path of the script that was called by the shell,
# or "" if it cannot be determined
get_startup_script <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_idx <- grep("^--file=", args)
  filename <- ""
  if (length(file_idx) == 1) {
    filename <- sub("^--file=", "", args[file_idx])
    filename <- normalizePath(filename)
  }
  filename
}

# Get the command-line arguments that were used to run this script
get_startup_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  args <- paste(args, collapse = " ")
  args
}
