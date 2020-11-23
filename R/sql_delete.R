#' SQL query to delete records
#'
#' @inheritParams sql_update
#'
#' @export
#' @examples
#' con <- DBI::dbConnect(RSQLite::SQLite(), tempfile())
#' sql_delete(
#'   data = df,
#'   table = "my_tbl",
#'   con = con,
#'   where = list("id1", SQL("my_tbl.value1 > 1")),
#'   returning = list(id = "id1", time = SQL("now()"))
#' )
sql_delete <- function(data,
                       table,
                       con,
                       where,
                       returning = NULL) {
  # TODO SQLite doesn't support `RETURNING` for `DELETE`
  # -> error and inform how to handle this
  check_standard_args(data, table, con)

  source_tbl <- "source"

  where_clause <- sql_clause_where(con, translate_where(con, where))

  delete_sql <- sql_statements(
    con = con,
    sql_clause_delete(con, ident(target = table)),
    sql_clause_where_exists(con, ident(source_tbl), where_clause, not = FALSE),
    if (length(returning)) sql_clause_returning(con, returning)
  )

  sql_with_clauses(
    con = con,
    sql_clause_data(con, data, source_tbl),
    delete_sql
  )
}
