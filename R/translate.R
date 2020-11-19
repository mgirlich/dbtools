translate_where <- function(con, where, target_tbl = "target", source_tbl = "source") {
  # compare to old `sql_clause_where()` -> need some checks etc
  # * names = name of column in table to update
  # * sql must not be named
  # * character can be named
  purrr::imap(
    auto_name(where),
    ~ {
      if (is_sql(.x)) {
        if (.y != "" && !is.na(.y)) {
          abort_invalid_input("SQL in `where` must not be named")
        }
        return(.x)
      }

      build_sql(
        sql_table_prefix(con, .y, ident(target_tbl)),
        " = ",
        sql_table_prefix(con, .x, ident(source_tbl)),
        con = con
      )
    }
  ) %>%
    purrr::flatten_chr() %>%
    sql()
}

translate_update <- function(con, update, source_tbl = "source") {
  # compare to old `sql_clause_update()` -> need some checks etc
  # * names = columns to be updated
  # * sql must be named
  # * character can be named
  purrr::imap(
    auto_name(update),
    ~ {
      if (is_sql(.x)) {
        if (.y == "") {
          abort_invalid_input("SQL in `update` must be named")
        }
        return(.x)
      }

      sql_table_prefix(con, .x, ident(source_tbl))
    }
  )
}
