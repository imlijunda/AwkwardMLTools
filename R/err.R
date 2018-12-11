err <- function(msg) {

  func <- as.character(sys.call(-2L))[1]

  err_msg <- sprintf("Error in %s: %s", func, msg)
  stop(err_msg)
}

err_null_arg <- function(arg) {

  arg_name <- as.character(substitute(arg))
  msg <- sprintf("NULL argument %s.", arg_name)

  err(msg)
}

err_invalid_value <- function(var, elaborate = NULL) {

  var_name <- as.character(substitute(var))
  msg <- sprintf("Invalid value of %s: %s", var_name, toString(var))
  if (!is.null(elaborate)) {
    msg <- paste0(msg, " ", elaborate)
  }

  err(msg)
}

err_invalid_class <- function(var, elaborate = NULL) {

  var_name <- as.character(substitute(var))
  msg <- sprintf("Invalid class of %s: %s", var_name, class(var))
  if (!is.null(elaborate)) {
    msg <- paste0(msg, " ", elaborate)
  }

  err(msg)
}

err_worker_null <- function(pid) {

  msg <- sprintf("Worker PID = %s returned NULL. Stopping all forked processes.", toString(pid))

  err(msg)
}

err_worker_stop <- function(pid) {

  msg <- sprintf("Workers PID = %s have already stopped.", toString(pid))

  err(msg)
}

err_worker <- function(info) {

  msg <- sprintf("Worker returned try-error: %s", toString(info))

  err(msg)
}
