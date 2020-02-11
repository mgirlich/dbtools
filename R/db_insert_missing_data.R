db_insert_missing_data <- function(data,
                                   table,
                                   con,
                                   conflict_target = NULL,
                                   insert_cols = NULL,
                                   returning = NULL,
                                   trans = TRUE,
                                   batch_size = 50e3) {
  batch_wise_db(
    data,
    con = con,
    .f = ~ {
      sql_insert_missing(
        from = .x,
        table = table,
        con = con,
        conflict_target = conflict_target,
        insert_cols = insert_cols,
        returning = returning
      )
    },
    trans = trans,
    returning = returning,
    batch_size = batch_size
  )
}
