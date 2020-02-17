#' On conflict DO NOTHING object
#'
#' @param conflict_target If `NULL` (the default) conflicts with all usable
#' constraints and unique indexes are handled. Otherwise a constraint name
#' given with `sql_constraint()` or a character vector of column names with
#' a unique index.
sql_do_nothing <- function(conflict_target = NULL) {
  new_conflict_clause(
    conflict_target,
    conflict_action = structure(
      list(),
      class = c("dbtools_conflict_do_nothing")
    )
  )
}

#' On conflict DO UPDATE object
#'
#' @param conflict_target A constraint name given with `sql_constraint()` or
#' a character vector of column names with a unique index.
sql_do_update <- function(conflict_target, update) {
  if (is_empty(conflict_target)) {
    abort_invalid_input("`conflict_target` must not be empty!")
  }

  new_conflict_clause(
    conflict_target,
    conflict_action = structure(
      update,
      class = c("dbtools_conflict_do_update")
    )
  )
}

new_conflict_clause <- function(conflict_target, conflict_action) {
  structure(
    list(
      conflict_target = conflict_target,
      conflict_action = conflict_action
    ),
    class = "dbtools_conflict_clause"
  )
}

#' @export
sql_constraint <- function(constraint) {
  if (!is_scalar_character(constraint)) {
    abort_invalid_input("constraint must be a scalar non-NA character.")
  }

  structure(constraint, class = "dbtools_constraint")
}

#' @export
sql_unique_cols <- function(...) {
  unique_cols <- c(...)
  if (is_empty(unique_cols) || !is.character(unique_cols) ||
      any(is.na(unique_cols))) {
    abort_invalid_input("... must be a non-empty non-NA character vector.")
  }

  structure(unique_cols, class = "dbtools_unique_cols")
}

is_unique_cols <- function(x) {
  inherits(x, "dbtools_unique_cols")
}
