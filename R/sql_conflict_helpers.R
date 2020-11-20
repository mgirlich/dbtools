#' On conflict DO NOTHING object
#'
#' @param conflict_target specifies the conflict target. This can be one of
#' the following:
#' * a character vector of column names that should be unique.
#' When using this together with mode "new" there must be a unique constraint
#' on these columns.
#' * a constraint name specified by [sql_constraint()].
#' @export
sql_do_nothing <- function(conflict_target) {
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
#' @inheritParams sql_do_nothing
#' @inheritParams sql_update
#' @export
sql_do_update <- function(conflict_target, update) {
  new_conflict_clause(
    conflict_target,
    conflict_action = structure(
      update,
      class = c("dbtools_conflict_do_update")
    )
  )
}

new_conflict_clause <- function(conflict_target, conflict_action) {
  if (!is_conflict_target(conflict_target)) {
    if (is_bare_character(conflict_target)) {
      conflict_target <- sql_unique_cols(conflict_target)
    } else {
      abort_invalid_input("must provide a valid conflict target")
    }
  }

  structure(
    list(
      conflict_target = conflict_target,
      conflict_action = conflict_action
    ),
    class = "dbtools_conflict_clause"
  )
}

#' SQL conflict target
#'
#' @param constraint The name of a constraint (a scalar character). The
#' constraint must be allowed to in use in the `ON CONFLICT ON CONSTRAINT`
#' clause.
#'
#' @name conflict-target
#' @export
sql_constraint <- function(constraint) {
  if (!is_scalar_character(constraint)) {
    abort_invalid_input("constraint must be a scalar non-NA character.")
  }

  new_conflict_target(constraint, class = "dbtools_constraint")
}

#' @param ... A character vector of the names of the unique columns.
#'
#' @name conflict-target
#' @export
sql_unique_cols <- function(...) {
  unique_cols <- c(...)
  if (is_empty(unique_cols) || !is.character(unique_cols) ||
      any(is.na(unique_cols))) {
    abort_invalid_input("... must be a non-empty non-NA character vector.")
  }

  new_conflict_target(unique_cols, class = "dbtools_unique_cols")
}

is_unique_cols <- function(x) {
  inherits(x, "dbtools_unique_cols")
}

new_conflict_target <- function(x, class) {
  structure(x, class = c(class, "dbtools_conflict_target"))
}

is_conflict_target <- function(x) {
  inherits(x, "dbtools_conflict_target")
}
