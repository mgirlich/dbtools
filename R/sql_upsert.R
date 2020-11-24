#' SQL for upserting values
#'
#' @inheritParams sql_insert_missing
#' @inheritParams sql_update
#'
#' @export
#' @examples
#' sql_upsert(
#'   data = data.frame(
#'     id = 1:2,
#'     value = c("a", "b"),
#'     value2 = 11:12
#'   ),
#'   table = "db_table",
#'   con = src_memdb2(),
#'   conflict_target = c("id"),
#'   update = list("value", updated_at = sql("now()"))
#' )
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
