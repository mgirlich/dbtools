#' SQL VALUES clause
#'
#' @export
#' @examples
#' sql_values(mtcars[1:3, c(1:3)], con)
sql_values <- function(data, con) {
  if (nrow(data) == 0) {
    # very hacky...
    # https://stackoverflow.com/questions/56957406/postgresql-empty-list-values-expression
    # ?? OR https://stackoverflow.com/questions/12426363/casting-null-type-when-updating-multiple-rows/12427434#12427434
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

sql_clause_from <- function(data, con, table) {
  UseMethod("sql_clause_from", data)
}

sql_clause_from.data.frame <- function(data, con, table) {
  values_clause <- sql_values(data, con)
  glue_sql("
    {`table`} ({`colnames(data)`*}) AS (
      {values_clause}
    )", .con = con)
}

sql_clause_from.character <- function(data, con, table) {
  stopifnot(length(data) == 1)
  glue_sql("{`data`} AS {`table`}", .con = con)
}

sql_clause_select <- function(x, con, table = "target") {
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

sql_clause_where <- function(where, con) {
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

sql_clause_update <- function(update, table_name, con) {
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

#' Add a returning clause
#'
#' @param sql An SQL clause.
#' @inheritParams sql_insert
#'
#' @export
add_sql_returning <- function(sql, returning, con) {
  if (is.null(returning)) {
    sql
  } else {
    returning_clause <- sql_clause_select(returning, con)
    glue_sql("{sql}\nRETURNING {returning_clause}", .con = con)
  }
}

sql_clause_generator <- function(x, expr_sql, expr_chr, collapse, ...) {
  data <- list(...)
  r <- purrr::map2(
    x, names2(x),
    ~ {
      if (is_sql(.x)) {
        e <- enexpr(expr_sql)
      } else {
        e <- enexpr(expr_chr)
      }
      eval_tidy(e, data = data)
    }
  )

  collapse_sql(r, collapse = collapse)
}
