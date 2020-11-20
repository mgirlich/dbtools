to_sql <- function(x, con) {
  UseMethod("to_sql", x)
}

#' @export
to_sql.dbtools_conflict_clause <- function(x, con) {
  conflict_action_sql <- to_sql(x$conflict_action, con)
  paste_sql(to_sql(x$conflict_target, con), conflict_action_sql, sep = " ")
}

#' @export
to_sql.dbtools_conflict_do_nothing <- function(x, con) {
  SQL("NOTHING")
}

#' @export
to_sql.dbtools_conflict_do_update <- function(x, con) {
  # update_clause <- sql_clause_update_old(x, SQL("EXCLUDED"), con)
  # glue_sql("UPDATE SET {update_clause}", .con = con)
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
