#' SQL VALUES clause
#'
#' @export
#' @examples
#' sql_values(mtcars[1:3, ], con)
sql_values <- function(data, con, table_name) {
  if (nrow(data) == 0) {
    # very hacky...
    # https://stackoverflow.com/questions/56957406/postgresql-empty-list-values-expression
    # ?? OR https://stackoverflow.com/questions/12426363/casting-null-type-when-updating-multiple-rows/12427434#12427434
    casts <- lapply(data, dbQuoteLiteral, conn = con)
    casts[lengths(casts) == 0] <- "::text"
    escaped_data <- collapse_sql(sub("^.*?(::.*)$", "ARRAY[]\\1[]", casts), ", ")
    from <- glue_sql("unnest({escaped_data})", .con = con)
  } else {
    escaped_data <- sqlData(con, data)

    vals <- purrr::pmap(
      escaped_data,
      ~ paste0("(", paste(..., sep = ", "), ")")
    ) %>%
      collapse_sql(",\n")
    from <- glue_sql("(VALUES {vals})", .con = con)
  }

  glue_sql("SELECT * FROM {from} AS {`table_name`} ({`colnames(data)`*})", .con = con)
}


sql_clause_from <- function(from, con, table_name, cols = NULL) {
  if (is.character(from)) {
    if (length(from) != 1) {
      abort("from must be a table name or a dataframe.")
    }

    # TODO support for cols?
    glue_sql("{`from`} AS {`table_name`}", .con = con)
  } else if (is.data.frame(from)) {
    if (!is_null(cols)) {
      from <- as.data.frame(from)[, cols, drop = FALSE]
    }
    sql_values(from, con, table_name)
  } else {
    abort("type not supported")
  }
}

sql_clause_select <- function(x, con, table = "target") {
  # character may be named, sql may be named
  if (!is_sql_chr_list(x, chr_names = NA, sql_names = NA) || is_null(x)) {
    abort_invalid_input("every element of returning must be a bare character or named bare SQL")
  }

  if (!is_null(table)) {
    table <- dbQuoteIdentifier(con, table)
    flag_sql <- purrr::map_lgl(x, is_sql)
    x[!flag_sql] <- paste_sql(table, ".", x[!flag_sql])
  }

  # add names
  quoted_names <- dbQuoteIdentifier(con, names2(x))
  names_flag <- names2(x) != ""
  x[names_flag] <- paste_sql(x, " AS ", quoted_names)[names_flag]
  collapse_sql(x, collapse = ",\n")
}

sql_clause_where <- function(where, con) {
  check_where(where)
  sql_clause_generator(
    auto_name(where),
    expr_sql = .x,
    expr_chr = glue_sql("target.{`.y`} = source.{`.x`}", .con = con),
    collapse = " AND ",
    con = con
  )
}

sql_clause_update <- function(update, table_name, con) {
  # character may be named, sql must be named
  if (!is_sql_chr_list(update, chr_names = NA, sql_names = TRUE)) {
    abort("every element of update must be a bare character or named bare SQL")
  }

  sql_clause_generator(
    auto_name(update),
    expr_sql = glue_sql("{`.y`} = {`.x`}", .con = con),
    expr_chr = glue_sql("{`.y`} = {`table_name`}.{`.x`}", .con = con),
    collapse = ",\n",
    con = con,
    table_name = table_name
  )
}

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
