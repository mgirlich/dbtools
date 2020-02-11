check_has_cols <- function(x, cols, x_arg = NULL, cols_arg = NULL) {
  x_arg <- x_arg %||% as_label(ensym(x))
  cols_arg <- cols_arg %||% as_label(ensym(cols))

  if (!all(has_name(x, cols))) {
    abort_missing_cols(
      object_name = x_arg,
      cols = setdiff(cols, colnames(x)),
      from = cols_arg
    )
  }
}


abort_dbtools <- function(message, error_type, ...) {
  abort(
    message = message,
    class = paste0("dbtools_error_", error_type),
    ...
  )
}

abort_missing_cols <- function(object_name, cols, from = NULL) {
  head_msg <- paste0(object_name, " is missing the following column(s)")
  if (!is_null(from)) {
    head_msg <- paste0(head_msg, " from ", from)
  }
  # body <- paste0("* ", cols, collapse = "\n")
  abort_dbtools(message = c(head_msg, cols), error_type = "missing_columns")
}
