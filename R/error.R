check_standard_args <- function(from, table, con) {
  stopifnot(is.data.frame(from) || is_bare_character(from, n = 1))
  stopifnot(is_bare_character(table, n = 1))
  stopifnot(inherits(con, "DBIConnection"))
}


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


check_unique_cols <- function(x, cols, x_arg = NULL, cols_arg = NULL) {
  x_arg <- x_arg %||% as_label(ensym(x))
  cols_arg <- cols_arg %||% as_label(ensym(cols))
  check_has_cols(x, cols, x_arg = x_arg, cols_arg = cols_arg)

  duplicated_flag <- duplicated(x[, cols])
  if (any(duplicated_flag)) {
    rows <- which(duplicated_flag)
    head <- paste0(
      x_arg, " has duplicates\n",
      "(i) Duplicates on columns ", cols_arg, " = ", deparse(unclass(cols)),
      "\n(i) ", length(rows), " rows with duplicates:"
    )
    abort_dbtools(
      message = c(head, shorten_error(rows)),
      error_type = "duplicates"
    )
  }
}


shorten_error <- function(x, n = 10) {
  if (length(x) > n) {
    c(x[1:n], "...")
  } else {
    x
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
