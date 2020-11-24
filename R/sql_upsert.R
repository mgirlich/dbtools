#' SQL for upserting values
#'
#' @inheritParams sql_insert_missing
#' @inheritParams sql_update
#'
#' @export
sql_upsert <- function(data,
                       table,
                       con,
                       conflict_target,
                       update,
                       insert_cols = NULL,
                       returning = NULL,
                       mode = "new") {
  sql_insert(
    data = data,
    table = table,
    con = con,
    conflict = sql_do_update(conflict_target, update),
    insert_cols = insert_cols,
    returning = returning,
    mode = mode
  )
}