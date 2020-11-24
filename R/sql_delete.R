#' SQL query to delete records
#'
#' @inheritParams sql_update
#'
#' @export
#' @examples
#' sql_delete(
#'   data = data.frame(
#'     row_id = 1:2,
#'     value = c("a", "b")
#'   ),
#'   table = "db_table",
#'   con = con_memdb(),
#'   where = list(id = "row_id", sql("target.updated = FALSE"))
#' )
sql_delete <- function(data,
                       table,
                       con,
                       where,
                       returning = NULL) {
  check_standard_args(data, table, con)
  check_supports_returning(con, returning)

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
