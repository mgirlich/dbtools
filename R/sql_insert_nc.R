#' SQL insert query
#'
#' @noRd
#' @examples
#' f <- function(...) {
#'   sql_insert_nc(df, "dbtools_test", con, ...)
#' }
#'
#' f(returning = sql("*"))
#'
#' f(conflict = sql_do_nothing(sql_unique_cols("id1", "id2")))
#'
#' f(
#'   conflict = sql_do_nothing(sql_unique_cols("id1", "id2")),
#'   returning = c("id1")
#' )
#'
#' f(
#'   conflict = sql_do_update(
#'     sql_unique_cols("id1", "id2"),
#'     update = list("value2", value1 = sql("target.value1 + 1"))
#'   ),
#'   returning = c("id1")
#' )
sql_insert_nc <- function(data,
                          table,
                          con,
                          conflict = NULL,
                          insert_cols = NULL,
                          returning = NULL,
                          return_all = FALSE
                          ) {
  if (!is_null(conflict) && !is_unique_cols(conflict$conflict_target)) {
    abort_invalid_input('cannot use constraint here for `mode = "old"`')
  }

  # TODO support `return_all` for `do_nothing` or conflict = NULL?
  # add_sql_return_all
  source_tbl <- "source"
  target_tbl <- "target"

  insert_cols <- insert_cols %||% colnames(data)

  insert_clause <- sql_insert_from_clauses(
    con = con,
    insert = sql_clause_insert_into(
      con,
      set_names(ident(table), target_tbl),
      ident(insert_cols)
    ),
    select = sql_clause_select(con, ident(insert_cols)),
    from = sql_clause_from(con, ident(source_tbl)),
    where = if (length(conflict)) sql_clause_do_nothing_nc(
      conflict$conflict_target,
      set_names(ident(table), target_tbl),
      con
    ),
    returning = sql_clause_returning(con, returning)
  )

  if (inherits(conflict$conflict_action, "dbtools_conflict_do_update")) {
    if (is_sqlite(con)) {
      message <- c(
        "upsert is not possible in one operation for SQLite.",
        i = 'use `mode = "new"` if there is a unique constraint;',
        i = "use `sql_update()` and then `sql_insert_missing()` otherwise"
      )
      abort_invalid_input(message)
    }

    update_clause <- sql_update(
      data = source_tbl,
      table = "dbtools_test",
      con = con,
      where = conflict$conflict_target,
      update = conflict$conflict_action
    )

    sql_with_clauses(
      con = con,
      sql_clause_data(con, data, source_tbl),
      sql_clause_cte_table(con, ident("insert_action"), insert_clause),
      # sql_clause_cte_table(con, ident("update_action"), update_clause),
      update_clause
    )

    # SELECT * FROM insert_action
    # UNION ALL
    # SELECT {sql_clause_select_old(returning, con)} FROM update_action

  } else {
    sql_with_clauses(
      con = con,
      sql_clause_data(con, data, source_tbl),
      insert_clause
    )
  }
}

sql_clause_do_nothing_nc <- function(conflict_target, table, con) {
  where_clause <- sql_clause_where(con, translate_where(con, conflict_target))
  sql_clause_where_exists(con, table, where_clause, not = TRUE)
}

sql_insert_mode <- function(mode) {
  switch (mode,
    "new" = sql_insert,
    "old" = sql_insert_nc
  )
}
