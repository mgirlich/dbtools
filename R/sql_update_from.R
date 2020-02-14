#' SQL query to update records
#'
#' @param table Name of the table to update.
#' @param con A DBIConnection object.
#' @param from Either the name of a database table (a scalar character) or
#' a dataframe to use for updating.
#' @param update Specifies which columns to update how.
#' @param where Expressions to use for WHERE clause
#' * character: a column name to join by. To join by different columns on
#' \<table\> and \<from\> use a named vector. For example `where = c("a" = "b")`
#' will match `from.a` to `table.b`.
#' * sql: must be unnamed
#' @param returning Expressions to return.
#' * character: column to return
#' * sql: expression to return
#' * names: name of column in returned result.
#'
#' @return An SQL query.
#' @export
sql_update <- function(from,
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
  check_standard_args(from, table, con)
  from_clause <- sql_clause_from(from, con, table_name = "source")

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
