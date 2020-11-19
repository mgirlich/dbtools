#' SQL query to update records
#'
#' @inheritParams default-args
#' @param update specifies the columns of `table` to update and its new values.
#' This can be one of the following:
#' * a character vector of column names, e.g. `c("a")` to update the
#' column a of `table` with the values of the column a `data`.
#' Names can be used to update a column with values from a column with a
#' different name. For example, `update = c(x = "y")` will column `x` of `table`
#' with the column `y` of `data`.
#' * a named list of scalar SQL (generated with [SQL()]. In the SQL code the
#' database table is named `target` and the input data is named `source`.
#' The name specifies the column to update. For example
#' `update = list(update_counter = SQL("target.update_counter + 1"))`
#' will increase the column update_counter by one.
#' * a mixture of these two: a list of scalar SQL and scalar character.
#' @param where specifies how to join `table` and `data`. This can be one of
#' the following:
#' * a character vector of column names to join on equal values of the
#' corresponding columns. To join by different variables on `table` and `data`
#' use a named vector. For example `where = c("a", x = "b")` will match
#' `table.a` to `data.a` and `table.x` to `table.b`
#' * an unnamed list of scalar SQL (generated with [SQL()]). In the SQL code the
#' database table is named `target` and the input data is named `source`.
#' * a mixture of these two. For example, when using
#' `where = list("name", SQL("target.country = 'de'"))`
#' all rows will be updated where name matches in `table` and `data` and
#' the column country of `table` is equal to `"de"`.
#' @param returning specifies the columns to return. If `NULL` (the default)
#' the number of updated/inserted rows are returned.
#' This can be one of the following:
#' * a character vector of column names.
#' * a list of scalar SQL (generated with [SQL()]). Note that only the
#' columns from `table` are visible, not the ones from `source`.
#' * a mixture of these two.
#' Names are used as the names of the returned columns. For example
#' `returning = list("id", time = SQL("now()"))`
#' will return a data.frame (or tibble if installed) with the columns
#' id and time.
#'
#' @return An SQL query. The returned rows are the ones that were updated.
#' @export
#' @examples
#' sql_update(
#'   data = mtcars,
#'   table = "target_table",
#'   con = src_memdb2(),
#'   update = list("value2", value1 = SQL("target.value1 + 1")),
#'   where = c("id1", "id2")
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

  if (is.data.frame(data)) {
    source_sql <- sql_clause_cte_table(
      con,
      ident(source_tbl),
      sql_values(data, con),
      columns = ident(colnames(data))
    )
  } else {
    source_sql <- NULL
  }

  update_clauses <- translate_update(con, update)
  where_clauses <- translate_where(con, where)

  update_clause <- sql_update_clauses(
    con,
    update = sql_clause_update(con, c(target = ident(table))),
    set = sql_clause_set(con, update_clauses),
    from = sql_clause_from(con, ident(source_tbl)),
    where = sql_clause_where(con, where_clauses),
    returning = sql_clause_returning(con, returning)
  )

  sql_with_clauses(
    con,
    if (length(source_sql)) source_sql,
    update_clause
  )
}
