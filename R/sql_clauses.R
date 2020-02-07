#' SQL VALUES clause
#'
#' @export
#' @examples
#' sql_values(mtcars[1:3, ], con)
sql_values <- function(data, con, table_name) {
  if (nrow(data) == 0) {
    # very hacky...
    # https://stackoverflow.com/questions/56957406/postgresql-empty-list-values-expression
    casts <- lapply(data, dbQuoteLiteral, conn = con)
    escaped_data <- ifelse(
      lengths(casts) > 0,
      paste0("ARRAY[]", casts, "[]"),
      "ARRAY[]::text[]"
    ) %>%
      collapse_sql(", ")
    inner <- glue_sql("unnest({escaped_data}) AS {`table_name`} ({`colnames(data)`*})", .con = con)
    glue_sql("(SELECT * FROM {inner})", .con = con)
  } else {
    escaped_data <- sqlData(con, data)

    vals <- purrr::pmap(
      escaped_data,
      ~ paste0("(", paste(..., sep = ", "), ")")
    ) %>%
      collapse_sql(",\n")

    glue_sql("(VALUES {vals}) AS {`table_name`} ({`colnames(data)`*})", .con = con)
  }
}


sql_from_clause <- function(from, con, table_name, cols = NULL) {
  if (is.character(from)) {
    if (length(from) != 1) {
      abort("from must be a table name or a dataframe.")
    }

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

sql_returning <- function(sql, returning, con) {
  if (is.null(returning)) {
    sql
  } else {
    returning_clause <- purrr::map2(
      returning, names2(returning),
      ~ {
        if (.y != "") {
          glue_sql("{`.x`} AS {`.y`}", .con = con)
        } else {
          glue_sql("{`.x`}", .con = con)
        }
      }
    ) %>%
      collapse_sql(collapse = ",\n")

    glue_sql("{sql}\nRETURNING {returning_clause}", .con = con)
  }
}

sql_clause_generator <- function(x, expr_sql, expr_chr, collapse, con = con) {
  r <- purrr::map2(
    x, names2(x),
    ~ {
      if (is_sql(.x)) {
        e <- enexpr(expr_sql)
      } else {
        e <- enexpr(expr_chr)
      }
      eval_tidy(e, data = list(con = con))
    }
  )

  collapse_sql(r, collapse = collapse)
}
