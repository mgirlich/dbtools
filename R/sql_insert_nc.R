#' SQL insert query
#'
#' @noRd
#' @examples
#' f <- function(...) {
#'   sql_insert_nc(df, "dbtools_test", con, ...)
#' }
#'
#' f(returning = SQL("*"))
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
#'     update = list("value2", value1 = SQL("target.value1 + 1"))
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
  # TODO support `return_all`?
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
      update_clause
    )
  } else {
    sql_with_clauses(
      con = con,
      sql_clause_data(con, data, source_tbl),
      insert_clause
    )
  }
}

sql_insert_nc_old <- function(data,
                              table,
                              con,
                              conflict = NULL,
                              insert_cols = NULL,
                              returning = NULL,
                              return_all = FALSE) {
  if (!is_null(conflict) && !is_unique_cols(conflict$conflict_target)) {
    abort_invalid_input('cannot use constraint here for `mode = "old"`')
  }

  from_clause <- sql_clause_from_old(data, con, table = "source")

  insert_cols <- auto_name(insert_cols)
  insert_sql <- sql_insert_from(
    data = "source",
    table = table,
    con = con,
    conflict = NULL,
    insert_cols = names(insert_cols),
    returning = NULL
  )%>%
    add_sql_conflict_nc(conflict, table, con) %>%
    sql_add_returning(returning, con)

  if (is_null(conflict) ||
      inherits(conflict$conflict_action, "dbtools_conflict_do_nothing")) {
    add_sql_return_all(
      insert_sql = insert_sql,
      from_clause = from_clause,
      table = table,
      return_all = return_all,
      returning = returning,
      conflict = conflict,
      con = con
    )
  } else {
    update_clause <- sql_update_old(
      data = "source",
      table = table,
      con = con,
      update = conflict$conflict_action,
      where = conflict$conflict_target
    )

    update_clause <- sql_clause_update_old(conflict$conflict_action, "source", con)
    update_query <- glue_sql("
      UPDATE {`table`} AS {`'target'`}
         SET {update_clause}
        FROM {`'source'`}
       WHERE {sql_clause_where_old(conflict$conflict_target, con)}
       ", .con = con)

    if (!is_null(returning)) {
      glue_sql("
        WITH {from_clause}
        , insert_action AS (
          {insert_sql}
        ), update_action AS (
          {update_query}
          RETURNING target.*
        )
        SELECT * FROM insert_action
        UNION ALL
        SELECT {sql_clause_select_old(returning, con)} FROM update_action
      ", .con = con)
    } else {
      glue_sql("
        WITH {from_clause}
        , insert_action AS (
          {insert_sql}
        )
        {update_query}
      ", .con = con)
    }
  }
}

sql_clause_do_nothing_nc <- function(conflict_target, table, con) {
  where_clause <- sql_clause_where(con, translate_where(con, where))
  sql_clause_where_not_exists(con, table, where_clause)
}

sql_clause_do_nothing_nc_old <- function(conflict_target, table, con) {
  where_clause <- sql_clause_where_old(conflict_target, con = con)
  glue_sql("
    WHERE NOT EXISTS (
     SELECT 1
       FROM {`table`} AS {`'target'`}
      WHERE {where_clause}
    )", .con = con)
}

add_sql_conflict_nc <- function(sql, conflict, table, con) {
  if (is_null(conflict)) {
    sql
  } else {
    conflict_sql <- sql_clause_do_nothing_nc_old(conflict$conflict_target, table, con)
    paste_sql(sql, "\n", conflict_sql)
  }
}

sql_insert_mode <- function(mode) {
  switch (mode,
    "new" = sql_insert,
    "old" = sql_insert_nc_old
  )
}
