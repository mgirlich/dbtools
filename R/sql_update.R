#' SQL query to update records
#'
#' @inheritParams default-args
#' @param update specifies the columns of `table` to update and its new values.
#' This can be one of the following:
#' * a character vector of column names, e.g. `c("a", "b")` to update the
#' columns `a` and `b` of `table` with the values of the columns `a` and `b` in
#' `data`.
#' To update with a different column, use a named vector. For example,
#' `update = c(x = "y")` will update column `x` of `table` with column `y` of `data`.
#' * a named SQL vector (generated with [sql()]. In the SQL code the
#' database table is named `target` and the input data is named `source`.
#' The name specifies the column to update. For example
#' `update = sql(update_counter = "target.update_counter + 1")`
#' will increase the column `update_counter` by one.
#' * a list of scalar SQL and scalar character.
#' @param where specifies how to join `table` and `data`. This can be one of
#' the following:
#' * a character vector of column names to join on equal values of the
#' corresponding columns. To join by different variables on `table` and `data`
#' use a named vector. For example `where = c("a", x = "b")` will match
#' `table.a` to `data.a` and `table.x` to `table.b`
#' * an unnamed SQL vector (generated with [sql()]). In the SQL code the
#' database table is named `target` and the input data is named `source`.
#' * a list of scalar SQL and scalar character. For example with
#' `where = list("name", sql("target.update_counter < 2"))`
#' all rows will be updated where the column `name` matches in `table` and
#' `data` and the column `update_counter` of `table` smaller than 2.
#' @param returning Specifies the columns to return. If `NULL` (the default)
#' you have to use [`DBI::dbExecute()`] to execute the SQL statement.
#'
#' If not `NULL` you have to use [`DBI::dbGetQuery()`] to get the updated rows.
#' `returning` can be one of the following:
#' * a (named) character vector of column names.
#' * a (named) SQL vector (generated with [sql()]). Note that only the
#' columns from `table` are visible, not the ones from `source`.
#' * a list of scalar SQL and scalar character.
#'
#' Names are used as the names of the returned columns. For example
#' `returning = list("id", time = sql("now()"))`
#' to return the column `id` and the current time in the column `time`.
#'
#' @return An SQL query.
#' @export
#' @examples
#' sql_update(
#'   data = data.frame(
#'     row_id = 1:2,
#'     value = c("a", "b")
#'   ),
#'   table = "db_table",
#'   con = con_memdb(),
#'   update = list("value", updated_at = sql("now()")),
#'   where = list(id = "row_id", sql("target.updated = FALSE"))
#' )
sql_update <- function(data,
                       table,
                       con,
                       update,
                       where,
                       returning = NULL) {
  if (!is_null(returning) & inherits(con, "SQLiteConnection")) {
    abort_invalid_input("`returning` doesn't work for SQLite")
  }

  # TODO why is `returning` handled so complicated in `sql_update_old()`?
  # see below:
  #
  # WITH {from_clause}
  # , ups AS (
  #   {update_query}
  #   RETURNING target.*
  # )
  # SELECT {select_clause}
  #   FROM ups

  # SQLite doesn't support an update from like syntax
  # --> have to use a subquery
  # see
  # https://stackoverflow.com/a/54323688/7529482
  # https://stackoverflow.com/questions/48690718/sqlite-update-column-from-column-in-another-table
  check_standard_args(data, table, con, from_table = TRUE)

  # TODO should `source_tbl` and `target_tbl` be arguments?
  source_tbl <- "source"
  target_tbl <- "target"

  update_clauses <- translate_update(con, update)
  where_clauses <- translate_where(con, where)

  update_clause <- sql_update_clauses(
    con,
    update = sql_clause_update(con, set_names(ident(table), target_tbl)),
    set = sql_clause_set(con, update_clauses),
    from = sql_clause_from(con, ident(source_tbl)),
    where = sql_clause_where(con, where_clauses),
    returning = sql_clause_returning(con, returning)
  )

  sql_with_clauses(
    con,
    sql_clause_data(con, data, source_tbl),
    update_clause
  )
}
