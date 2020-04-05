#' Disable cometr logging
#'
#' Generally, if the `COMET_LOGGING_FILE` and `COMET_LOGGING_FILE_LEVEL` parameters
#' are found, then `cometr` will log internal information. You can disable logging
#' for a particular R session by calling `disable_logging()`.
#' @export
disable_logging <- function() {
  .cometrenv$cache$canlog <- FALSE
  invisible(NULL)
}

LOG_DEBUG <- function(..., echo = FALSE) {
  comet_log(..., level = "DEBUG", echo = echo)
}

LOG_INFO <- function(..., echo = FALSE) {
  comet_log(..., level = "INFO", echo = echo)
}

LOG_ERROR <- function(..., echo = FALSE) {
  comet_log(..., level = "ERROR", echo = echo)
}

comet_log <- function(..., level, echo = FALSE) {
  text <- paste(list(...), collapse = "")
  if (echo) {
    cat(text, "\n")
  }

  comet_log_helper(text = text, level = level)
}

comet_log_helper <- function(text, level) {
  tryCatch({
    if (can_write_log()) {
      requested_level <- .cometrenv$LOG_LEVEL_MAP[[level]]
      config_level <- .cometrenv$LOG_LEVEL_MAP[[get_config_logging_file_level()]]
      if (requested_level >= config_level) {
        write(
          sprintf("%s [%s] %s",
                  get_human_time(),
                  .cometrenv$LOG_LEVEL_SHORTHAND[[level]],
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
  if (is.null(.cometrenv$cache$canlog)) {
    .cometrenv$cache$canlog <- can_write_log_helper()
  }
  .cometrenv$cache$canlog
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

  if (!loglevel %in% names(.cometrenv$LOG_LEVEL_MAP)) {
    warning("`COMET_LOGGING_FILE_LEVEL` must be one of \"",
            paste(names(.cometrenv$LOG_LEVEL_MAP), collapse = "\", \""), "\".", call. = FALSE)
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
