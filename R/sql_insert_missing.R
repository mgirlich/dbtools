#' SQL for inserting missing values
#'
#' @inheritParams sql_insert
#' @inheritParams sql_update
#' @inheritParams sql_do_nothing
#'
#' @return An SQL query.
#' @export
#' @examples
#' sql_insert_missing(
#'   data = mtcars[c(1, 3), ],
#'   table = "dbtools_mtcars",
#'   con = con,
#'   conflict_target = c("mpg", "cyl"),
#'   returning = sql("*"),
#'   return_all = TRUE
#' )
sql_insert_missing <- function(data,
                               table,
                               con,
                               conflict_target,
                               insert_cols = NULL,
                               returning = NULL,
                               return_all = FALSE,
                               mode = "new") {
  sql_insert(
    data = data,
    table = table,
    con = con,
    conflict = sql_do_nothing(conflict_target),
    insert_cols = insert_cols,
    returning = returning,
    return_all = return_all,
    mode = mode
  )
}