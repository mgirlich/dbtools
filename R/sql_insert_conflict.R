new_conflict_clause <- function(conflict_target, conflict_action) {
  structure(
    list(
      conflict_target = conflict_target,
      conflict_action = conflict_action
    ),
    class = "dbtools_conflict_clause"
  )
}

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
sql_do_update <- function(conflict_target, updates) {
  if (is_empty(conflict_target)) {
    abort("conflict_target must not be empty!")
  }

  new_conflict_clause(
    conflict_target,
    conflict_action = structure(
      updates,
      class = c("dbtools_conflict_do_update")
    )
  )
}


#' SQL for inserting missing values
#'
#' @export
#' @examples
#' DBI::dbWriteTable(con, "dbtools_mtcars", mtcars[1, ], overwrite = TRUE)
#' DBI::dbExecute(
#'   con,
#'   'ALTER TABLE dbtools_mtcars
#'   ADD CONSTRAINT unique_const
#'   UNIQUE ("mpg", "cyl");')
#' sql_insert_missing(
#'   from = mtcars[c(1, 3), ],
#'   table = "dbtools_mtcars",
#'   con = con,
#'   conflict_target = sql_conflict_cols("mpg", "cyl"),
#'   returning = sql("*"),
#'   return_all = TRUE
#' )
sql_insert_missing <- function(from,
                               table,
                               con,
                               conflict_target = NULL,
                               insert_cols = NULL,
                               returning = NULL,
                               return_all = FALSE) {
  # NOTE only rows that were succesfully inserted or updated are returned
  # see https://stackoverflow.com/questions/36083669/get-id-from-a-conditional-insert/36090746#36090746
  # --> might want to use this to return all rows
  sql_insert(
    from = from,
    table = table,
    con = con,
    conflict = sql_do_nothing(conflict_target),
    insert_cols = insert_cols,
    returning = returning
  )
}


#' SQL for inserting missing values
#'
#' @param conflict_target Name of a constraint or name of columns with a
#' unique constraint.
#'
#' @export
sql_upsert <- function(from,
                       table,
                       con,
                       conflict_target,
                       updates,
                       insert_cols = NULL,
                       returning = NULL) {
  # TODO check updates --> same as in sql_update?
  sql_insert(
    table = table,
    con = con,
    from = from,
    conflict = sql_do_update(conflict_target, updates),
    insert_cols = insert_cols,
    returning = returning
  )
}

#' @export
sql_constraint <- function(constraint) {
  if (!is_scalar_character(constraint)) {
    abort_invalid_input("constraint must be a scalar non-NA character.")
  }

  structure(constraint, class = "dbtools_constraint")
}

to_sql.dbtools_constraint <- function(x, con) {
  constraint <- DBI::dbQuoteIdentifier(con, x)
  paste_sql("ON CONSTRAINT ", constraint)
}

#' @export
sql_conflict_cols <- function(...) {
  conflict_cols <- c(...)
  if (is_empty(conflict_cols) || !is.character(conflict_cols) ||
      any(is.na(conflict_cols))) {
    abort_invalid_input("... must be a non-empty non-NA character vector.")
  }

  structure(conflict_cols, class = "dbtools_conflict_cols")
}

is_conflict_cols <- function(x) {
  inherits(x, "dbtools_conflict_cols")
}

to_sql.dbtools_conflict_cols <- function(x, con) {
  conflict_cols <- DBI::dbQuoteIdentifier(con, x)
  conflict_cols_vec <- paste0(conflict_cols, collapse = ", ")
  paste_sql("(", conflict_cols_vec, ")")
}
