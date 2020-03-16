LOG_DEBUG <- function(...) {
  comet_log(..., level = "DEBUG")
}

LOG_INFO <- function(...) {
  comet_log(..., level = "INFO")
}

LOG_ERROR <- function(...) {
  comet_log(..., level = "ERROR")
}

comet_log <- function(..., level) {
  text <- paste(list(...), collapse = "")

  comet_log_helper(text = text, level = level)
}

comet_log_helper <- function(text, level) {
  tryCatch({
    if (can_write_log()) {
      requested_level <- .cometenv$LOG_LEVEL_MAP[[level]]
      config_level <- .cometenv$LOG_LEVEL_MAP[[get_config_logging_file_level()]]
      if (requested_level >= config_level) {
        write(
          sprintf("%s [%s] %s",
                  get_human_time(),
                  .cometenv$LOG_LEVEL_SHORTHAND[[level]],
                  text),
          file = get_config_logging_file(),
          append = TRUE
        )
      }
    }
  }, error = function(err) {
    warning("Failed to write cometr to log: ", err$message, call. = FALSE)
  })
}

can_write_log <- function() {
  if (is.null(.cometenv$cache$canlog)) {
    .cometenv$cache$canlog <- can_write_log_helper()
  }
  .cometenv$cache$canlog
}

can_write_log_helper <- function() {
  logfile <- get_config_logging_file()
  loglevel <- get_config_logging_file_level()
  if (is.null(logfile) && is.null(loglevel)) {
    return(FALSE)
  }
  if (xor(is.null(logfile), is.null(loglevel))) {
    warning("You must set both `COMET_LOGGING_FILE` and `COMET_LOGGING_FILE_LEVEL` ",
      "in order to log cometr activity.", call. = FALSE)
    return(FALSE)
  }

  if (is.null(loglevel)) {
    return(FALSE)
  }
  if (!loglevel %in% names(.cometenv$LOG_LEVEL_MAP)) {
    warning("`COMET_LOGGING_FILE_LEVEL` must be one of \"",
            paste(names(.cometenv$LOG_LEVEL_MAP), collapse = "\", \""), "\".", call. = FALSE)
    return(FALSE)
  }

  if (is.null(logfile)) {
    return(FALSE)
  }
  if (!file.exists(logfile)) {
    success <- try(file.create(logfile, showWarnings = FALSE), silent = TRUE)
    if ("try-error" %in% class(success) || !success) {
      warning("Couldn't create log file ", logfile, call. = FALSE)
      return(FALSE)
    }
  }
  if (file.access(logfile, mode = 2)[[1]] != 0) {
    warning("Cannot write to log file ", logfile, call. = FALSE)
    return(FALSE)
  }

  TRUE
}

get_human_time <- function(time = Sys.time()) {
  format(time, "%Y-%m-%d %H:%M:%S")
}

comet_stop <- function(...) {
  LOG_ERROR(...)
  stop(..., call. = FALSE)
}

comet_warning <- function(...) {
  LOG_ERROR(...)
  warning(..., call. = FALSE)
}
