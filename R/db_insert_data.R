#' Insert data to a database table
#'
#' @inheritParams sql_insert
#' @param trans perform operation in a transaction?
#' @param batch_size number of rows to process in a single statement.
#'
#' @export
db_insert_data <- function(data,
                           table,
                           con,
                           insert_cols = NULL,
                           returning = NULL,
                           trans = TRUE,
                           batch_size = 50e3) {
  # NOTE copy = TRUE now seems to have little performance gain
  # NOTE copy = TRUE needed (2019-11-22) as copy = FALSE doesn't work with characters
  # NOTE parametrized queries insert one by one, very slow
  # NOTE use RPostgres::dbWriteTable for higher speed when not returning anything?

  batch_wise_db(
    data = data,
    con = con,
    .f = ~ sql_insert(
      data = .x,
      table = table,
      con = con,
      insert_cols = insert_cols,
      returning = returning
    ),
    trans = trans,
    returning = returning,
    batch_size = batch_size
  )
}
