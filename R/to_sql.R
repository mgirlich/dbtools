to_sql <- function(x, con) {
  UseMethod("to_sql", x)
}

#' @export
to_sql.dbtools_conflict_do_nothing <- function(x, con) {
  sql("NOTHING")
}

#' @export
to_sql.dbtools_conflict_do_update <- function(x, con) {
  update_clauses <- translate_update(con, x, source_tbl = "EXCLUDED")
  build_sql(
    "UPDATE ", sql_clause_set(con, update_clauses),
    con = con
  )
}

#' @export
to_sql.dbtools_unique_cols <- function(x, con) {
  unique_cols <- DBI::dbQuoteIdentifier(con, x)
  unique_cols_vec <- paste0(unique_cols, collapse = ", ")
  paste_sql("CONFLICT (", unique_cols_vec, ")")
}

#' @export
to_sql.dbtools_constraint <- function(x, con) {
  constraint <- DBI::dbQuoteIdentifier(con, x)
  paste_sql("CONSTRAINT ", constraint)
}
