#' Auto name a list or character vector
#'
#' @noRd
#' @examples
#' \dontrun{
#' auto_name(list("body"))
#' list("body", left = "right", sql("unnamed_sql"), left = sql("named_sql")) %>%
#'   auto_name()
#' }
auto_name_chr <- function(x) {
  chr_flag <- purrr::map_lgl(x, vec_is, character())
  update_flag <- !have_name(x) & chr_flag

  nms <- names2(x)
  nms[update_flag] <- purrr::map_chr(x[update_flag], as_name)
  set_names(x, nms)
}

is_postgres <- function(conn) {
  inherits(conn, "PostgreSQLConnection") ||
    inherits(conn, "PqConnection") ||
    inherits(conn, "PostgreSQL")
}

is_sqlite <- function(conn) {
  inherits(conn, "SQLiteConnection")
}

maybe_as_tibble <- function(x) {
  if (is_installed("tibble")) {
    tibble::as_tibble(x)
  } else {
    x
  }
}

indent <- function(x) {
  stringi::stri_paste("  ", stringr::str_replace_all(x, "\\n", "\n  "))
}

src_memdb2 <- function() {
  dbplyr::src_memdb()$con
}

memdb_frame2 <- function(..., .name) {
  if (DBI::dbExistsTable(src_memdb2(), .name)) {
    DBI::dbRemoveTable(src_memdb2(), .name)
  }
  dbplyr::memdb_frame(..., .name = .name)
}

pg_frame2 <- function(..., .name) {
  if (DBI::dbExistsTable(con_pg, .name)) {
    DBI::dbRemoveTable(con_pg, .name)
  }
  DBI::dbWriteTable(con_pg, .name, tibble::tibble(...))
}

is_named2 <- function(x) {
  is_empty(x) || is_named(x)
}

#' @export
`[.sql` <- function(x, i, ...) {
  sql(NextMethod("["))
}

#' @export
`[[.sql` <- function(x, i, ...) {
  sql(NextMethod("[["))
}
