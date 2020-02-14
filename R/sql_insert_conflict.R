#' SQL for inserting missing values
#'
#' @export
#' @examples
#' DBI::dbWriteTable(con, "dbtools_mtcars", mtcars[1, ], overwrite = TRUE)
#' DBI::dbExecute(
#'   con,
#'   'ALTER TABLE dbtools_mtcars
#'   ADD CONSTRAINT unique_const
#'   UNIQUE ("mpg", "cyl");'
#' )
#' sql_insert_missing(
#'   from = mtcars[c(1, 3), ],
#'   table = "dbtools_mtcars",
#'   con = con,
#'   conflict_target = sql_conflict_cols("mpg", "cyl"),
#'   returning = SQL("*"),
#'   return_all = TRUE
#' )
sql_insert_missing <- function(from,
                               table,
                               con,
                               conflict_target = NULL,
                               insert_cols = NULL,
                               returning = NULL,
                               return_all = FALSE) {
  sql_insert(
    from = from,
    table = table,
    con = con,
    conflict = sql_do_nothing(conflict_target),
    insert_cols = insert_cols,
    returning = returning,
    return_all = return_all
  )
}


#' SQL for inserting missing values
#'
#' @param conflict_target Name of a constraint or name of columns with a
#' unique constraint.
#'
#' @export
sql_upsert <- function(from,
                       table,
                       con,
                       conflict_target,
                       updates,
                       insert_cols = NULL,
                       returning = NULL) {
  sql_insert(
    table = table,
    con = con,
    from = from,
    conflict = sql_do_update(conflict_target, updates),
    insert_cols = insert_cols,
    returning = returning
  )
}
