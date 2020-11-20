# idea from `dbplyr` package
#
# the `sql_clause_*()` functions generate simple SQL clauses, e.g.
#
# ```
# con <- dbplyr::simulate_postgres()
# sql_clause_from(con, "my table")
# #> <SQL> FROM 'my table'
# ```

sql_with_clauses <- function(con,
                             ...) {
  dots <- purrr::compact(list(...))
  n <- length(dots)

  if (n == 1) {
    dots[[1]]
  } else {
    with_clauses <- sql_vector(
      dots[-n],
      parens = FALSE,
      collapse = ",\n",
      con = con
    )

    build_sql(
      "WITH ", with_clauses, "\n",
      dots[[n]],
      con = con
    )
  }
}

sql_update_clauses <- function(con,
                               update,
                               set,
                               from,
                               where,
                               returning = NULL) {
  sql_statements(
    con,
    update,
    set,
    from,
    where,
    returning
  )
}

sql_insert_from_clauses <- function(con,
                                    insert,
                                    select,
                                    from,
                                    where = NULL,
                                    on_conflict = NULL,
                                    returning = NULL) {
  sql_statements(
    con,
    insert,
    select,
    from,
    where,
    on_conflict,
    returning
  )
}

sql_statements <- function(con, ...) {
  parts <- purrr::compact(list(...))
  # escape(unname(parts), collapse = "\n", parens = FALSE, con = con)
  sql(paste0(parts, collapse = "\n"))
}

# SQL clauses -------------------------------------------------------------

sql_clause_data <- function(con, data, table) {
  check_standard_args(data, table, con, from_table = TRUE)
  UseMethod("sql_clause_data", data)
}

#' @export
sql_clause_data.data.frame <- function(con, data, table) {
  values_clause <- sql_values(data, con)
  sql_clause_cte_table(
    con,
    ident(table),
    values_clause,
    columns = ident(colnames(data))
  )
  # glue_sql("
  #   {`table`} ({`colnames(data)`*}) AS (
  #     {values_clause}
  #   )", .con = con)
}

#' @export
sql_clause_data.character <- function(con, data, table) {
  stopifnot(length(data) == 1)
  # glue_sql("{`data`} AS {`table`}", .con = con)
  NULL
}

#' @noRd
#' @examples
#' sql_clause_cte_table(
#'   con,
#'   "update_table",
#'   sql("SELECT * FROM df"),
#'   columns = c("col 1", "col 2")
#' )
sql_clause_cte_table <- function(con, table, select_clause, columns = NULL) {
  table <- escape(table, con = con)

  if (is_null(columns)) {
    cte_table <- table
  } else {
    columns <- escape(columns, con = con)
    cte_table <- build_sql(
      table, " ",
      sql_vector(columns, parens = TRUE, collapse = ", ", con = con),
      con = con
    )
  }

  build_sql(
    cte_table, " AS (\n",
    # "  ", select_clause, "\n",
    sql(indent(select_clause)), "\n",
    ")",
    con = con
  )
}

sql_clause_from  <- function(con, from) {
  sql_clause_generic(con, "FROM", from)
}

sql_clause_update <- function(con, table) {
  sql_clause_generic(con, "UPDATE", table)
}

sql_clause_delete <- function(con, table) {
  sql_clause_generic(con, "DELETE FROM", table)
}

sql_clause_where_not_exists <- function(con, table, where_clause) {
  build_sql(
    "WHERE NOT EXISTS (\n",
    "  SELECT 1\n",
    "    FROM ", table, "\n",
    "   WHERE ", where_clause, "\n",
    ")",
    con = con
  )
}

#' @noRd
#' @examples
#' sql_clause_insert_into(
#'   con,
#'   c(target = "test"),
#'   ident("id1", "id2", "value1")
#' )
sql_clause_insert_into <- function(con, table, columns) {
  columns_esc <- escape(columns, parens = FALSE, con = con)
  build_sql(
    "INSERT INTO ", table,
    " ", sql_vector(columns_esc, parens = TRUE, collapse = ", ", con = con),
    con = con
  )
}

sql_clause_set <- function(con, updates) {
  if (!is_named(updates)) {
    abort_invalid_input("`updates` must be named.")
  }

  # compare to `sql_clause_where()`
  updates_esc <- purrr::imap(updates, ~ build_sql(ident(.y), " = ", .x, con = con))
  updates_esc <- unname(updates_esc)

  build_sql(
    "SET ", sql_vector(updates_esc, parens = FALSE, collapse = ", ", con = con),
    con = con
  )
}

sql_clause_select <- function(con, select) {
  assert_that(is.character(select))
  if (is_empty(select)) {
    abort("Query contains no columns")
  }

  build_sql(
    "SELECT ",
    escape(select, collapse = ", ", con = con),
    con = con
  )
}

sql_clause_where <- function(con, where) {
  if (length(where) == 0L) {
    return()
  }

  assert_that(is.character(where))
  where_paren <- escape(where, parens = TRUE, con = con)
  build_sql(
    "WHERE ", sql_vector(where_paren, collapse = " AND ", con = con),
    con = con
  )
}

sql_clause_returning <- function(con, returning) {
  # TODO what classes to expect from `returning`?
  # * sql -> do nothing
  # * character -> treat as fields?

    if (is_null(returning)) {
    return(NULL)
  }
  # assert_that(is.character(returning))

  build_sql(
    "RETURNING ",
    escape(returning, collapse = ", ", con = con),
    con = con
  )
}

# helper ------------------------------------------------------------------

sql_clause_generic <- function(con, clause, fields) {
  if (length(fields) == 0L) {
    return()
  }

  assert_that(is.character(fields))
  build_sql(
    sql(clause), " ",
    escape(fields, collapse = ", ", con = con),
    con = con
  )
}
