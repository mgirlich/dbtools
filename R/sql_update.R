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
  check_standard_args(data, table, con, from_table = TRUE)
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
