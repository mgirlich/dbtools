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
  from_clause <- sql_clause_from(data, con, table = "source")

  glue_sql("
    WITH {from_clause}
    DELETE FROM {`table`} AS target
     WHERE EXISTS (
        SELECT *
          FROM source
         WHERE {sql_clause_where(where, con)}
    )
     ", .con = con) %>%
    sql_add_returning(returning, con)
}
