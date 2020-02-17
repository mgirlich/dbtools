#' Update a database table
#'
#' @section Unnecessary Updates:
#' Be careful when doing updates very frequently on a table. On an update
#' operation Postgres writes a new version of a row even when nothing has
#' changed (due to its [MVCC model](https://www.postgresql.org/docs/current/mvcc-intro.html)).
#' This means that frequently updating a table without changing a value can
#' still bloat the table. For example this can easily happen together when
#' updating a timestamp column via a trigger.
#'
#' @return If `returning` is `NULL` the number of rows updated; Otherwise a
#' tibble of the updated rows and the columns as specified in `returning`..
#' @export
db_upsert_data <- function(data,
                           table,
                           con,
                           update,
                           conflict_target,
                           insert_cols = NULL,
                           returning = NULL,
                           trans = TRUE,
                           batch_size = 50e3,
                           mode = "new") {
  batch_wise_db(
    data,
    con = con,
    .f = ~ {
      sql_upsert(
        data = .x,
        table = table,
        con = con,
        update = update,
        conflict_target = conflict_target,
        insert_cols = insert_cols,
        returning = returning,
        mode = mode
      )
    },
    trans = trans,
    returning = returning,
    batch_size = batch_size
  )
}
