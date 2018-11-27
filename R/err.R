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
    msg <- paste0(msg, " -- ", elaborate)
  }

  err(msg)
}
