#' Delete data from a database table
#'
#' @inheritParams sql_delete
#' @inheritParams db_insert_data
#'
#' @export
db_delete_data <- function(data,
                           table,
                           con,
                           where,
                           returning = NULL,
                           trans = TRUE,
                           batch_size = 50e3) {
  batch_wise_db(
    data,
    con = con,
    .f = ~ {
      sql_delete(
        data = .x,
        table = table,
        con = con,
        where = where,
        returning = returning
      )
    },
    trans = trans,
    returning = returning,
    batch_size = batch_size
  )
}
