#' SQL query to delete records
#'
#' @export
#' @examples
#' sql_delete(
#'   table = "my_tbl",
#'   con = con,
#'   from = "my_value_table",
#'   where = list("where 1", SQL("my_tbl.id > 1")),
#'   returning = list(`ret 1` = "ret_col", SQL("now()"))
#' )
sql_delete <- function(from,
                       table,
                       con,
                       where,
                       returning = NULL) {
  check_standard_args(from, table, con)
  from_clause <- sql_clause_from(from, con, table = "source")

  glue_sql("
    WITH {from_clause}
    DELETE FROM {`table`} AS target
     WHERE EXISTS (
        SELECT *
          FROM source
         WHERE {sql_clause_where(where, con)}
    )
     ", .con = con) %>%
    add_sql_returning(returning, con)
}
