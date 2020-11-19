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
  check_standard_args(data, table, con)

  where_clause <- sql_clause_where_old(where, con)

  delete_sql <- sql_statements(
    con = con,
    sql_clause_delete(con, ident(target = table)),
    sql_clause_where_not_exists(con, ident("source"), where_clause),
    if (length(returning)) sql_clause_returning(con, returning)
  )

  sql_with_clauses(
    con = con,
    if (is.data.frame(data)) sql_clause_from_old(data, con, table = "source"),
    delete_sql
  )
}
