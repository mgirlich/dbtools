check_standard_args <- function(data, table, con, from_table = FALSE) {
  # if (is_true(from_table)) {
  #   stopifnot(is.data.frame(data) || is_bare_character(data, n = 1))
  # } else {
  #   stopifnot(is.data.frame(data))
  # }
  stopifnot(is.data.frame(data) || is_bare_character(data, n = 1))
  stopifnot(is_bare_character(table, n = 1))
  stopifnot(inherits(con, "DBIConnection"))
}


check_has_cols <- function(x, cols, x_arg = NULL, cols_arg = NULL) {
  if (is.character(x)) {
    return()
  }

  x_arg <- x_arg %||% as_label(enexpr(x))
  cols_arg <- cols_arg %||% as_label(enexpr(cols))

  if (!all(has_name(x, cols))) {
    abort_missing_cols(
      object_name = x_arg,
      cols = setdiff(cols, colnames(x)),
      from = cols_arg
    )
  }
}


check_supports_returning <- function(con, returning) {
  if (is_sqlite(con) && !is_null(returning)) {
    abort_invalid_input("`returning` doesn't work for SQLite")
    # TODO more information how to work around?
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


check_sql_chr_list <- function(x, x_arg = as_label(ensym(x))) {
  header <- glue::glue("`{x_arg}` must be an SQL expression list")

  if (is_null(x) || !(is_character(x) || is_bare_list(x))) {
    x_class <- class(x)
    abort_invalid_input(paste0(header, ", not ", x_class))
  }

  # check types
  type_problems <- purrr::map(
    x,
    ~ {
      if (is_bare_character(.x) || is_scalar_sql(.x)) {
        if (length(.x) != 1) {
          paste0("size must be 1, no ", length(.x))
        }
      } else {
        paste0("class must be SQL or character, not ", class(.x))
      }
    }
  )

  problem_flag <- lengths(type_problems) > 0
  if (any(problem_flag)) {
    abort_invalid_input(c(header, elt_message(problem_flag, type_problems)))
  }
}

check_sql_names <- function(x, named, x_arg = as_label(ensym(x))) {
  x_sql_flag <- purrr::map_lgl(x, is_scalar_sql)
  x_name_flag <- names2(x) != ""
  problem_flag <- x_name_flag != named & x_sql_flag

  if (any(problem_flag)) {
    if (named) {
      predicate <- ""
      details <- "does not have a name"
    } else {
      predicate <- "not "
      details <- "has a name"
    }

    header <- paste0(
      "Every SQL element of ", x_arg, " must ", predicate, "be named.\n",
      "Problems at"
    )
    abort_invalid_input(c(header, elt_message(problem_flag, details)))
  }
}


elt_message <- function(problem_flag, details) {
  positions <- which(problem_flag)
  paste0("element ", positions, ": ", details[problem_flag])
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

abort_invalid_input <- function(message) {
  abort_dbtools(
    message = message,
    error_type = "invalid_input"
  )
}
