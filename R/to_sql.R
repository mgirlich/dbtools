to_sql <- function(x, con) {
  UseMethod("to_sql")
}

to_sql.dbtools_conflict_clause <- function(x, con) {
  conflict_action_sql <- to_sql(x$conflict_action, con)

  if (!is.null(x$conflict_target)) {
    paste_sql(to_sql(x$conflict_target, con), conflict_action_sql, sep = " ")
  } else {
    conflict_action_sql
  }
}

to_sql.dbtools_conflict_do_nothing <- function(x, con) {
  sql("DO NOTHING")
}

to_sql.dbtools_conflict_do_update <- function(x, con) {
  update_clause <- sql_clause_generator(
    auto_name(x),
    expr_sql = glue_sql("{`.y`} = {`.x`}", .con = con),
    expr_chr = glue_sql("{`.y`} = EXCLUDED.{`.x`}", .con = con),
    collapse = ",\n",
    con = con
  )

  glue_sql("DO UPDATE SET {update_clause}", .con = con)
}
