#' SQL query to update records
#'
#' @param table Name of the table to update.
#' @inheritParams sql_insert
#' @param update Specifies which columns to update how.
#' @param where Expressions to use for WHERE clause
#' * character: a column name to join by. To join by different columns on
#' \<table\> and \<from\> use a named vector. For example `where = c("a" = "b")`
#' will match `from.a` to `table.b`.
#' * sql: must be unnamed
#'
#' @return An SQL query.
#' @export
sql_update <- function(data,
                       table,
                       con,
                       update,
                       where,
                       returning = NULL) {
  # SQLite doesn't support an update from like syntax
  # --> have to use a subquery
  # see
  # https://stackoverflow.com/a/54323688/7529482
  # https://stackoverflow.com/questions/48690718/sqlite-update-column-from-column-in-another-table
  check_standard_args(data, table, con)
  from_clause <- sql_clause_from(data, con, table = "source")

  # create update clause
  update_clause <- sql_clause_update(update, "source", con)
  update_query <- glue_sql("
    UPDATE {`table`} AS {`'target'`}
       SET {update_clause}
      FROM source
     WHERE {sql_clause_where(where, con)}
     ", .con = con)

  if (is_null(returning)) {
    glue_sql("
      WITH {from_clause}
      {update_query}
    ", .con = con)
  } else {
    select_clause <- sql_clause_select(returning, con)
    glue_sql("
      WITH {from_clause}
      , ups AS (
        {update_query}
        RETURNING target.*
      )
      SELECT {select_clause}
        FROM ups
    ", .con = con)
  }
}
