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
  # TODO return values:
  # a) returning = NULL
  #   1) nrow(data) > 0 --> # rows affected (i.e. sum over results)
  #   2) nrow(data) = 0 --> 0
  # b) returning != NULL
  #   1) nrow(data) > 0 --> tibble with columns as in returning and rows from
  #       the updated rows
  #   2) nrow(data) = 0 --> should return corresponding tibble but how?
  # TODO upload only required columns

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
