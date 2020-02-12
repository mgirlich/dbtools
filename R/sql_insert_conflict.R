#' SQL for inserting missing values
#'
#' @export
#' @examples
#' DBI::dbWriteTable(con, "dbtools_mtcars", mtcars[1, ], overwrite = TRUE)
#' DBI::dbExecute(
#'   con,
#'   'ALTER TABLE dbtools_mtcars
#'   ADD CONSTRAINT unique_const
#'   UNIQUE ("mpg", "cyl");')
#' sql_insert_missing(
#'   from = mtcars[c(1, 3), ],
#'   table = "dbtools_mtcars",
#'   con = con,
#'   conflict_target = sql_conflict_cols("mpg", "cyl"),
#'   returning = sql("*"),
#'   return_all = TRUE
#' )
sql_insert_missing <- function(from,
                               table,
                               con,
                               conflict_target = NULL,
                               insert_cols = NULL,
                               returning = NULL,
                               return_all = FALSE) {
  # NOTE only rows that were succesfully inserted or updated are returned
  # see https://stackoverflow.com/questions/36083669/get-id-from-a-conditional-insert/36090746#36090746
  # --> might want to use this to return all rows
  sql_insert(
    from = from,
    table = table,
    con = con,
    conflict = sql_do_nothing(conflict_target),
    insert_cols = insert_cols,
    returning = returning
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
  # TODO check updates --> same as in sql_update?
  sql_insert(
    table = table,
    con = con,
    from = from,
    conflict = sql_do_update(conflict_target, updates),
    insert_cols = insert_cols,
    returning = returning
  )
}
