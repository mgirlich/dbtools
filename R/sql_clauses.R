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
  # character may be named, sql may be named
  if (!is_sql_chr_list(returning, chr_names = NA, sql_names = NA) &&
    !is.null(returning)) {
    abort("every element of returning must be a bare character or named bare SQL")
  }

  if (is.null(returning)) {
    sql
  } else {
    returning_clause <- purrr::map2(
      returning, names2(returning),
      ~ {
        if (!is_sql(.x)) {
          .x <- glue_sql("target.{`.x`}", .con = con)
        }
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
