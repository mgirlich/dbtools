#' Common arguments
#'
#' @param data A data frame or the name of a database table.
#' @param table The name of the database table to update, insert to, or upsert
#' to. Can be an unquoted character string or an identifier created with [ident()].
#' @param con A DBIConnection object, as returned by [DBI::dbConnect()].
#' @name default-args
NULL

#' SQL insert query
#'
#' The function powering `sql_insert_missing()` and `sql_upsert()`. Usually,
#' there is no need to call this function directly.
#'
#' @inheritParams default-args
#' @param conflict Specifies the action on a given conflict. If `NULL` (the
#' default) conflicts produce an error. If given then it must be generated
#' by [sql_do_update()] or [sql_do_nothing()].
#' @param insert_cols Columns from `data` to insert.
#' @inheritParams sql_update
#' @param return_all A boolean that specifies whether only newly inserted rows
#' (`FALSE`, the default) are returned or also the matching existing rows.
#' @param mode Specify how to check for a conflict:
#' * "new": use the SQL "ON CONFLICT" clause.
#' * "old": do not use the "ON CONFLICT" clause but an anti-join.
#'
#' @return An SQL query.
#' @export
#' @examples
#' sql_insert(
#'   data = iris[1:2, ],
#'   table = "iris_tbl",
#'   con = con_memdb(),
#'   conflict = sql_do_nothing("Species"),
#'   insert_cols = c("Species", "Sepal.Length", "Sepal.Width"),
#'   returning = list(width = "Sepal.Width", time = sql("now()"))
#' )
sql_insert <- function(data,
                       table,
                       con,
                       conflict = NULL,
                       insert_cols = NULL,
                       returning = NULL,
                       return_all = FALSE,
                       mode = "new") {
  check_standard_args(data, table, con)
  stopifnot(is_bare_character(insert_cols) || is_null(insert_cols))
  stopifnot(is_null(conflict) || inherits(conflict, "dbtools_conflict_clause"))

  # check insert cols + conflict cols
  insert_cols <- insert_cols %||% colnames(data)
  check_has_cols(data, insert_cols)

  if (is_unique_cols(conflict$conflict_target)) {
    check_has_cols(data, conflict$conflict_target)
  }

  check_supports_returning(con, returning)

  f_insert <- switch(
    mode,
    new = sql_insert_on_conflict,
    old = sql_insert_nc
  )

  f_insert(
    data = data,
    table = table,
    con = con,
    conflict = conflict,
    insert_cols = insert_cols,
    returning = returning,
    return_all = return_all
  )
}

sql_insert_on_conflict <- function(data,
                                   table,
                                   con,
                                   conflict = NULL,
                                   insert_cols = NULL,
                                   returning = NULL,
                                   return_all = FALSE) {
  source_tbl <- "source"
  target_tbl <- "target"
  data_tbl <- ident_data(data, source_tbl)

  # TODO handling of empty insert_cols isn't nice
  # --> maybe do not allow them?

  # `WHERE true` is needed for SQLite
  # see: https://modern-sql.com/blog/2019-01/sqlite-in-2018#upsert
  insert_clause <- sql_insert_from_clauses(
    con = con,
    insert = sql_clause_insert_into(
      con,
      set_names(ident(table), target_tbl),
      ident(insert_cols)
    ),
    select = sql_clause_select(con, if (is_empty(insert_cols)) sql("*") else ident(insert_cols)),
    from = sql_clause_from(con, data_tbl),
    where = if (length(conflict)) sql_clause_where(con, sql("true")),
    on_conflict = if (length(conflict)) translate_conflict(con, conflict),
    returning = sql_clause_returning(con, returning)
  )

  if (is_true(return_all)) {
    sql_with_clauses(
      con = con,
      sql_clause_data(con, data, source_tbl),
      sql_clause_cte_table(con, ident("ins_result"), insert_clause),
      sql_return_all(con, table, returning, conflict, data_tbl)
    )
  } else {
    sql_with_clauses(
      con = con,
      sql_clause_data(con, data, source_tbl),
      insert_clause
    )
  }
}

# sql_return_all(
#   con,
#   table = "my table",
#   returning = c("mpg", "cyl"),
#   conflict = sql_do_nothing(c("id", "id 2"))
# )
sql_return_all <- function(con, table, returning, conflict, source_tbl) {
  if (is_null(returning)) {
    message <- c(
      "`return_all` is `TRUE` but not specified what to return",
      i = "specify what to return with `returning` argument"
    )
    abort_invalid_input(message)
  }

  if (!is_unique_cols(conflict$conflict_target)) {
    message <- c(
      "`return_all` is `TRUE` but a constraint is used",
      i = "use unique columns instead"
    )
    abort_invalid_input(message)
  }

  target_tbl <- "target"

  # idea from
  # https://stackoverflow.com/questions/35949877/how-to-include-excluded-rows-in-returning-from-insert-on-conflict/35953488#35953488
  # https://stackoverflow.com/questions/36083669/get-id-from-a-conditional-insert/36090746#36090746
  where_clause <- sql_clause_where(con, translate_where(con, conflict$conflict_target))
  sql_statements(
    con,
    sql_clause_select(con, sql("*")),
    sql_clause_from(con, ident("ins_result")),
    "UNION ALL",
    sql_clause_select(con, maybe_ident(returning)),
    sql_clause_from(con, set_names(ident(table), target_tbl)),
    sql_clause_where_exists(con, source_tbl, where_clause, not = FALSE)
  )
}
