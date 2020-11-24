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
    with_clauses <- sql_statements(con, !!!dots[-n])

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
  parts <- purrr::compact(list2(...))
  collapse_sql(parts, collapse = "\n")
}

# SQL clauses -------------------------------------------------------------

sql_clause_data <- function(con, data, table) {
  check_standard_args(data, table, con, from_table = TRUE)
  UseMethod("sql_clause_data", data)
}

#' @export
sql_clause_data.data.frame <- function(con, data, table) {
  values_clause <- sql_values(con, data)
  sql_clause_cte_table(
    con,
    ident(table),
    values_clause,
    columns = ident(colnames(data))
  )
}

#' @export
sql_clause_data.character <- function(con, data, table) {
  stopifnot(length(data) == 1)
  NULL
}

#' SQL VALUES clause
#'
#' `sql_values()` translates a data frame to an SQL VALUES clause.
#'
#' @details
#' Because a `VALUES` clause must have at least one row a `data.frame` with
#' zero rows is translated as a `SELECT` clause that returns zero rows.
#'
#' @param data A data frame.
#' @inheritParams default-args
#'
#' @return An SQL clause (a scalar object of class SQL).
#'
#' @export
#' @examples
#' sql_values(con_memdb(), mtcars[1:3, c(1:3)])
#'
#' # zero row data frames produce a SELECT query
#' sql_values(con_memdb(), mtcars[0, c(1:3)])
sql_values <- function(con, data) {
  stopifnot(is.data.frame(data))
  stopifnot(inherits(con, "DBIConnection"))

  if (nrow(data) == 0) {
    # very hacky...
    # https://stackoverflow.com/questions/56957406/postgresql-empty-list-values-expression
    # https://stackoverflow.com/questions/12426363/casting-null-type-when-updating-multiple-rows/12427434#12427434
    if (is_postgres(con)) {
      # nocov start
      casts <- lapply(data, DBI::dbQuoteLiteral, conn = con)
      casts[lengths(casts) == 0] <- "::text"
      escaped_data <- collapse_sql(sub("^.*?(::.*)$", "NULL\\1", casts), ", ")
      # nocov end
    } else if (is_sqlite(con)) {
      escaped_data <- collapse_sql(rep_along(data, "NULL"), ", ")
    }

    paste_sql("SELECT ", escaped_data, " WHERE FALSE")
  } else {
    escaped_data <- DBI::sqlData(con, data)

    vals <- purrr::pmap(
      escaped_data,
      ~ paste0("(", paste(..., sep = ", "), ")")
    ) %>%
      collapse_sql(",\n  ")

    paste_sql("VALUES\n", "  ", vals)
  }
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
  table <- escape(maybe_ident(table), con = con)

  if (is_null(columns)) {
    cte_table <- table
  } else {
    columns <- escape(maybe_ident(columns), con = con)
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
  sql_clause_generic(con, "UPDATE", maybe_ident(table))
}

sql_clause_delete <- function(con, table) {
  sql_clause_generic(con, "DELETE FROM", maybe_ident(table))
}

sql_clause_where_exists <- function(con, table, where_clause, not) {
  build_sql(
    "WHERE ", if (is_true(not)) sql("NOT "), "EXISTS (\n",
    "  SELECT 1\n",
    "    ", sql_clause_from(con, table), "\n",
    "   ", where_clause, "\n",
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
    "INSERT INTO ", maybe_ident(table),
    " ", sql_vector(columns_esc, parens = TRUE, collapse = ", ", con = con),
    con = con
  )
}

sql_clause_set <- function(con, updates) {
  if (!is_named(updates)) {
    abort_invalid_input("`updates` must be named.")
  }

  if (!is_sql(updates)) {
    abort_invalid_input("`updates` must be sql.")
  }

  # compare to `sql_clause_where()`
  updates_esc <- purrr::imap(updates, ~ build_sql(ident(.y), " = ", .x, con = con))
  updates_esc <- unname(updates_esc)

  build_sql(
    "SET\n  ", sql_vector(updates_esc, parens = FALSE, collapse = ",\n  ", con = con),
    con = con
  )
}

sql_clause_select <- function(con, select) {
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


#' SQL RETURNING clause
#'
#' `sql_clause_returning()` adds a `RETURNING` clause to an existing SQL clause.
#'
#' @inheritParams sql_insert
#'
#' @return An SQL clause (a scalar object of class SQL). If `returning` is
#' `NULL` then `NULL` is also returned.
sql_clause_returning <- function(con, returning) {
  if (is_null(returning)) {
    return(NULL)
  }

  returning <- maybe_ident(as.list(returning))

  build_sql(
    "RETURNING ",
    escape(returning, parens = FALSE, collapse = ", ", con = con),
    con = con
  )
}

sql_clause_on_conflict <- function(con, conflict_target, conflict_action) {
  build_sql(
    "ON ", conflict_target, "\n",
    "DO ", conflict_action,
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
