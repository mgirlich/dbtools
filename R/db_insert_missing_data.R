#' @export
db_insert_missing_data <- function(data,
                                   table,
                                   con,
                                   conflict_target = NULL,
                                   insert_cols = NULL,
                                   returning = NULL,
                                   return_all = FALSE,
                                   trans = TRUE,
                                   batch_size = 50e3,
                                   mode = "new") {
  if (is_unique_cols(conflict_target)) {
    check_unique_cols(data, conflict_target)
  }

  batch_wise_db(
    data,
    con = con,
    .f = ~ {
      sql_insert_missing(
        data = .x,
        table = table,
        con = con,
        conflict_target = conflict_target,
        insert_cols = insert_cols,
        returning = returning,
        return_all = return_all,
        mode = mode
      )
    },
    trans = trans,
    returning = returning,
    batch_size = batch_size
  )
}
