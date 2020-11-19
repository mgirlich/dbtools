#' Generate an SQL VALUES clause
#'
#' `sql_values()` translates a data.frame to an SQL VALUES clause.
#'
#' @details
#' Because a `VALUES` clause must have at least one row a `data.frame` with
#' zero rows is translated as a `SELECT` clause that returns zero rows.
#'
#' @inheritParams default-args
#'
#' @return An SQL clause (a scalar object of class SQL).
#'
#' @export
#' @examples
#' con <- DBI::dbConnect(RSQLite::SQLite(), tempfile())
#' sql_values(mtcars[1:3, c(1:3)], con)
#' sql_values(mtcars[0, c(1:3)], con)
sql_values <- function(data, con) {
  stopifnot(is.data.frame(data))
  stopifnot(inherits(con, "DBIConnection"))

  if (nrow(data) == 0) {
    # very hacky...
    # https://stackoverflow.com/questions/56957406/postgresql-empty-list-values-expression
    # https://stackoverflow.com/questions/12426363/casting-null-type-when-updating-multiple-rows/12427434#12427434
    if (is_postgres(con)) {
      casts <- lapply(data, DBI::dbQuoteLiteral, conn = con)
      casts[lengths(casts) == 0] <- "::text"
      escaped_data <- collapse_sql(sub("^.*?(::.*)$", "NULL\\1", casts), ", ")
    } else if (is_sqlite(con)) {
      escaped_data <- collapse_sql(rep_along(data, "NULL"), ", ")
    }

    paste_sql("SELECT ", escaped_data, " WHERE FALSE")
  } else {
    escaped_data <- sqlData(con, data)

    vals <- purrr::pmap(
      escaped_data,
      ~ paste0("(", paste(..., sep = ", "), ")")
    ) %>%
      collapse_sql(",\n  ")

    paste_sql("VALUES\n", "  ", vals)
  }
}

sql_clause_from_old <- function(data, con, table) {
  check_standard_args(data, table, con, from_table = TRUE)
  UseMethod("sql_clause_from_old", data)
}

#' @export
sql_clause_from_old.data.frame <- function(data, con, table) {
  # TODO this is actually not a from clause but more like a with clause...
  values_clause <- sql_values(data, con)
  glue_sql("
    {`table`} ({`colnames(data)`*}) AS (
      {values_clause}
    )", .con = con)
}

#' @export
sql_clause_from_old.character <- function(data, con, table) {
  stopifnot(length(data) == 1)
  glue_sql("{`data`} AS {`table`}", .con = con)
}

sql_clause_select_old <- function(x, con, table = "target") {
  check_sql_chr_list(x, "returning")

  if (!is_null(table)) {
    table <- dbQuoteIdentifier(con, table)
    flag_sql <- purrr::map_lgl(x, is_sql)
    x[!flag_sql] <- paste_sql(
      table, ".",
      dbQuoteIdentifier(con, unlist(x[!flag_sql]))
    )
  }

  # add names
  quoted_names <- dbQuoteIdentifier(con, names2(x))
  names_flag <- names2(x) != ""
  x[names_flag] <- paste_sql(x, " AS ", quoted_names)[names_flag]
  collapse_sql(x, collapse = ",\n")
}

sql_clause_where_old <- function(where, con) {
  .x <- NULL
  check_sql_chr_list(where, "where")
  check_sql_names(where, FALSE, "where")

  sql_clause_generator(
    auto_name(where),
    expr_sql = .x,
    expr_chr = glue_sql("target.{`.y`} = source.{`.x`}", .con = con),
    collapse = " AND ",
    con = con
  )
}

sql_clause_update_old <- function(update, table_name, con) {
  check_sql_chr_list(update)
  check_sql_names(update, TRUE, "update")

  sql_clause_generator(
    auto_name(update),
    expr_sql = glue_sql("{`.y`} = {`.x`}", .con = con),
    expr_chr = glue_sql("{`.y`} = {`table_name`}.{`.x`}", .con = con),
    collapse = ",\n",
    con = con,
    table_name = table_name
  )
}

#' Add a RETURNING clause
#'
#' `sql_add_returning()` adds a `RETURNING` clause to an existing SQL clause.
#'
#' @param sql An SQL clause.
#' @inheritParams sql_insert
#'
#' @return An SQL clause (a scalar object of class SQL). If `returning` is
#' `NULL` then `sql` is returned as is.
#'
#' @export
sql_add_returning <- function(sql, returning, con) {
  stopifnot(is_sql(sql))

  if (is.null(returning)) {
    sql
  } else {
    returning_clause <- sql_clause_select_old(returning, con)
    glue_sql("{sql}\nRETURNING {returning_clause}", .con = con)
  }
}

sql_clause_generator <- function(x, expr_sql, expr_chr, collapse, ...) {
  data <- list(...)
  expr_sql <- enexpr(expr_sql)
  expr_chr <- enexpr(expr_chr)
  r <- purrr::map2(
    x, names2(x),
    ~ {
      if (is_sql(.x)) {
        e <- expr_sql
      } else {
        e <- expr_chr
      }
      eval_tidy(e, data = data)
    }
  )

  collapse_sql(r, collapse = collapse)
}
