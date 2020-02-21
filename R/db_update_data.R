#' Update a database table
#'
#' @inheritParams sql_update
#' @inheritParams db_insert_data
#'
#' @export
db_update_data <- function(data,
                           table,
                           con,
                           update,
                           where,
                           returning = NULL,
                           trans = TRUE,
                           batch_size = 50e3) {
  batch_wise_db(
    data,
    con = con,
    .f = ~ {
      sql_update(
        data = .x,
        table = table,
        con = con,
        update = update,
        where = where,
        returning = returning
      )
    },
    trans = trans,
    returning = returning,
    batch_size = batch_size
  )
}
