#' SQL expression list
#'
#' SQL expressions are used in the following arguments
#' * `update` (in [db_update_data()] and [db_upsert_data()])
#' * `where` (in [db_delete_data()], and [db_update_data()])
#' * `returning` (in [db_delete_data()], [db_insert_data()], [db_insert_missing_data()], [db_update_data()], and [db_upsert_data()])
#'
#' An SQL expression list is either a character vector e.g. `c("id", "name")`.
#' Or it can be a list where each element is
#' * a scalar character, i.e. a character of length 1 which must not be `NA`.
#' * a SQL class generated with [DBI::SQL()] or [glue::glue_sql()].
#'
#' A scalar character refers to a column in the dataframe and of the database
#' table. If the element is named then the name is used for the database.
#' SQL elements are not translated but left as is. In an SQL expression
#' the table to be updated is referred to as `target` and the input data
#' as `source`.
#'
#' @name sql-expression-list
NULL

#' Common arguments
#'
#' @param data a data.frame.
#' @param table name of the database table.
#' @param con a DBIConnection object, as returned by [DBI::dbConnect()].
#' @name default-args
NULL

#' SQL insert query
#'
#' @inheritParams default-args
#' @param conflict specifies the action on a given conflict. If `NULL` (the
#' default) conflicts produce an error. If given then it must be generated
#' by [sql_do_update()] or [sql_do_nothing()].
#' @param insert_cols columns from `data` to insert.
#' @inheritParams sql_update
#' @param return_all a boolean that specifies whether only newly inserted rows
#' (`FALSE`, the default) are returned or also the matching, existing rows.
#' @param mode specify how to check for a conflict:
#' * "new": use the SQL "ON CONFLICT" clause.
#' * "old": do not use the "ON CONFLICT" clause but an anti-join.
#'
#' @return An SQL query.
#' @export
#' @examples
#' sql_insert(
#'   data = iris[1:2, ],
#'   table = "iris_tbl",
#'   con = con,
#'   conflict = sql_do_nothing("Species"),
#'   insert_cols = c("Species", "Sepal.Length", "Sepal.Width"),
#'   returning = list(width = "Sepal.Width", time = SQL("now()"))
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
  # TODO return_all
  source_tbl <- "source"
  target_tbl <- "target"

  # `WHERE true` is needed for SQLite
  # see: https://modern-sql.com/blog/2019-01/sqlite-in-2018#upsert
  insert_clause <- sql_insert_from_clauses(
    con = con,
    insert = sql_clause_insert_into(
      con,
      set_names(ident(table), target_tbl),
      ident(insert_cols)
    ),
    select = sql_clause_select(con, ident(insert_cols)),
    from = sql_clause_from(con, ident(source_tbl)),
    where = if (length(conflict)) sql_clause_where(con, sql("true")),
    on_conflict = if (length(conflict)) translate_conflict(con, conflict),
    returning = sql_clause_returning(con, returning)
  )

  sql_with_clauses(
    con = con,
    sql_clause_data(con, data, source_tbl),
    insert_clause
  )

  # add_sql_return_all(
  #   insert_sql = insert_sql,
  #   from_clause = from_clause,
  #   table = table,
  #   return_all = return_all,
  #   returning = returning,
  #   conflict = conflict,
  #   con = con
  # )
}

add_sql_return_all <- function(insert_sql, from_clause, table,
                               return_all, returning, conflict, con) {
  if (is_true(return_all)) {
    if (is_null(returning)) {
      message <- c(
        "`return_all` is `TRUE` but not specified what to return",
        "specify what to return with `returning` argument"
      )
      abort_invalid_input(message)
    }

    if (!is_unique_cols(conflict$conflict_target)) {
      message <- c(
        "`return_all` is `TRUE` but a constraint is used",
        "use unique columns instead"
      )
      abort_invalid_input(message)
    }

    # idea from
    # https://stackoverflow.com/questions/35949877/how-to-include-excluded-rows-in-returning-from-insert-on-conflict/35953488#35953488
    # https://stackoverflow.com/questions/36083669/get-id-from-a-conditional-insert/36090746#36090746
    glue_sql("
      WITH {from_clause}
      , ins_result AS (
        {insert_sql}
      )
      SELECT *
        FROM ins_result
      UNION ALL
      SELECT {sql_clause_select(returning, con)}
        FROM {`table`} AS {`'target'`}
       WHERE EXISTS (
         SELECT 1
           FROM source
          WHERE {sql_clause_where(conflict$conflict_target, con)}
       )
    ", .con = con)
  } else {
    glue_sql("
      WITH {from_clause}
      {insert_sql}
    ", .con = con)
  }
}
