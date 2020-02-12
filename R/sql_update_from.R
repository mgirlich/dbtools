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
  check_standard_args(from, table, con)
  from <- sql_clause_from(from, con, table_name = "source")

  # create update clause
  # character may be named, sql must be named
  if (!is_sql_chr_list(update, chr_names = NA, sql_names = TRUE)) {
    abort("every element of update must be a bare character or named bare SQL")
  }

  update_clause <- sql_clause_generator(
    auto_name(update),
    expr_sql = glue_sql("{`.y`} = {`.x`}", .con = con),
    expr_chr = glue_sql("{`.y`} = source.{`.x`}", .con = con),
    collapse = ",\n",
    con = con
  )

  # create where clause
  check_where(where)
  where_clause <- sql_clause_generator(
    auto_name(where),
    expr_sql = .x,
    expr_chr = glue_sql("target.{`.y`} = source.{`.x`}", .con = con),
    collapse = " AND ",
    con = con
  )

  # create returning clause
  # character may be named, sql may be named
  if (!is_sql_chr_list(returning, chr_names = NA, sql_names = NA) &&
    !is.null(returning)) {
    abort("every element of returning must be a bare character or named bare SQL")
  }

  glue_sql("
    UPDATE {`table`} AS {`'target'`}
       SET {update_clause}
      FROM {`from`}
     WHERE {where_clause}
     ", .con = con) %>%
    sql_returning(returning, con)
}
