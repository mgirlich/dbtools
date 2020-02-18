to_sql <- function(x, con) {
  UseMethod("to_sql", x)
}

to_sql.dbtools_conflict_clause <- function(x, con) {
  conflict_action_sql <- to_sql(x$conflict_action, con)
  paste_sql(to_sql(x$conflict_target, con), conflict_action_sql, sep = " ")
}

to_sql.dbtools_conflict_do_nothing <- function(x, con) {
  SQL("DO NOTHING")
}

to_sql.dbtools_conflict_do_update <- function(x, con) {
  update_clause <- sql_clause_update(x, SQL("EXCLUDED"), con)

  glue_sql("DO UPDATE SET {update_clause}", .con = con)
}

to_sql.dbtools_unique_cols <- function(x, con) {
  unique_cols <- DBI::dbQuoteIdentifier(con, x)
  unique_cols_vec <- paste0(unique_cols, collapse = ", ")
  paste_sql("(", unique_cols_vec, ")")
}

to_sql.dbtools_constraint <- function(x, con) {
  constraint <- DBI::dbQuoteIdentifier(con, x)
  paste_sql("ON CONSTRAINT ", constraint)
}
