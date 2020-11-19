translate_where <- function(con, where, target_tbl = "target", source_tbl = "source") {
  # compare to old `sql_clause_where()` -> need some checks etc
  # * names = name of column in table to update
  # * sql must not be named
  # * character can be named
  where_list <- auto_name_chr(cast_to_list(where))
  sql_flag <- are_sql(where_list)

  if (any(have_name(where_list[sql_flag]))) {
    abort_invalid_input("SQL in `where` must not be named")
  }

  where_list[!sql_flag] <- purrr::imap(
    where_list[!sql_flag],
    ~ {
      build_sql(
        sql_table_prefix(con, .y, ident(target_tbl)),
        " = ",
        sql_table_prefix(con, .x, ident(source_tbl)),
        con = con
      )
    }
  )

  sql(purrr::flatten_chr(where_list))
}

translate_update <- function(con, update, source_tbl = "source") {
  # compare to old `sql_clause_update()` -> need some checks etc
  # * names = columns to be updated
  # * sql must be named
  # * character can be named
  update_list <- auto_name_chr(cast_to_list(update))
  sql_flag <- are_sql(update_list)

  if (!is_named2(update_list[sql_flag])) {
    abort_invalid_input("SQL in `update` must be named")
  }

  update_list[!sql_flag] <- purrr::imap(
    update_list[!sql_flag],
    ~ sql_table_prefix(con, .x, ident(source_tbl))
  )

  update_list
}

cast_to_list <- function(x) {
  if (is_sql(x)) {
    purrr::map(x, sql)
  } else {
    as.list(x)
  }
}
