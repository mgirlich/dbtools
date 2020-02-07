#' SQL query to delete records
#'
#' @export
#' @examples
#' sql_delete(
#'   table = "my_tbl",
#'   con = con,
#'   from = "my_value_table",
#'   where = list("where 1", sql("my_tbl.id > 1")),
#'   returning = list(`ret 1` = "ret_col", sql("now()"))
#' )
sql_delete <- function(from,
                       table,
                       con,
                       where,
                       returning = NULL) {
  from <- sql_from_clause(from, con)

  # create where clause
  # character may be named, sql must not be named
  if (!is_sql_chr_list(where, chr_names = NA, sql_names = FALSE)) {
    abort("every element of where must be a bare character or unnamed bare SQL")
  }

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
    DELETE FROM {`table`} AS target
     USING {`from`} AS source
     WHERE {where_clause}
     ", .con = con
  ) %>%
    sql_returning(returning, con)
}
