#' SQL VALUES clause
#'
#' @export
#' sql_values(mtcars[1:3, ], con)
sql_values <- function(data, con) {
  escaped_data <- sqlData(con, data)

  vals <- purrr::pmap(
    escaped_data,
    ~ paste0("(", paste(..., sep = ", "), ")")
  ) %>%
    collapse_sql(",\n")

  glue_sql("(VALUES {vals}) AS t ({`colnames(data)`*})", .con = con)
}


sql_from_clause <- function(from, con) {
  if (is.character(from)) {
    if (length(from) != 1) {
      abort("from must be a table name or a dataframe.")
    }
    DBI::dbQuoteIdentifier(con, from)
  } else if (is.data.frame(from)) {
    sql_values(from, con)
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

sql_clause_generator <- function(x, expr_sql, expr_chr, collapse) {
  r <- purrr::map2(
    x, names2(x),
    ~ {
      if (is_sql(.x)) {
        eval_tidy(enexpr(expr_sql))
      } else {
        eval_tidy(enexpr(expr_chr))
      }
    }
  )

  collapse_sql(r, collapse = collapse)
}
