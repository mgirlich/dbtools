#' @noRd
#' @param where
#' * names = name of column in target table
#' * character can be named
#' * sql must not be named
translate_where <- function(con, where, target_tbl = "target", source_tbl = "source") {
  where_list <- auto_name_chr(as.list(where))
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

  flatten_sql(where_list, names = FALSE)
}

#' @noRd
#' @param where
#' * names = columns to be updated
#' * character can be named
#' * sql must be named
translate_update <- function(con, update, source_tbl = "source") {
  update_list <- auto_name_chr(as.list(update))
  sql_flag <- are_sql(update_list)

  if (!is_named2(update_list[sql_flag])) {
    abort_invalid_input("SQL in `update` must be named")
  }

  update_list[!sql_flag] <- purrr::imap(
    update_list[!sql_flag],
    ~ sql_table_prefix(con, .x, ident(source_tbl))
  )

  flatten_sql(update_list, names = TRUE)
}

translate_conflict <- function(con, conflict) {
  sql_clause_on_conflict(
    con,
    conflict_target = to_sql(conflict$conflict_target, con),
    conflict_action = to_sql(conflict$conflict_action, con)
  )
}

#' @export
as.list.sql <- function(x, ...) {
  purrr::map(x, sql)
}

#' @export
as.list.SQL <- function(x, ...) {
  purrr::map(x, SQL)
}

flatten_sql <- function(x, names) {
  x_out <- sql(purrr::flatten_chr(x))
  if (is_true(names)) {
    x_out <- set_names(x_out, names2(x))
  }

  x_out
}
