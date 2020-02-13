to_sql <- function(x, con) {
  UseMethod("to_sql", x)
}

to_sql.dbtools_conflict_clause <- function(x, con) {
  conflict_action_sql <- to_sql(x$conflict_action, con)

  if (!is.null(x$conflict_target)) {
    if (!inherits_any(x$conflict_target, c("dbtools_conflict_cols", "dbtools_constraint"))) {
      abort("conflict_target must be generated with sql_conflict_cols() or sql_constraint().")
    }
    paste_sql(to_sql(x$conflict_target, con), conflict_action_sql, sep = " ")
  } else {
    conflict_action_sql
  }
}

to_sql.dbtools_conflict_do_nothing <- function(x, con) {
  sql("DO NOTHING")
}

to_sql.dbtools_conflict_do_update <- function(x, con) {
  update_clause <- sql_clause_update(x, "EXCLUDED", con)

  glue_sql("DO UPDATE SET {update_clause}", .con = con)
}
