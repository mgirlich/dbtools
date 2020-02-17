#' Update a database table
#'
#' @return If `returning` is `NULL` the number of rows updated; Otherwise a
#' tibble of the updated rows and the columns as specified in `returning`..
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
