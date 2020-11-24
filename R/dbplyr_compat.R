# nocov start
sql_table_prefix <- function(con, var, table = NULL) {
  var <- sql_escape_ident(con, var)
  if (!is.null(table)) {
    table <- sql_escape_ident(con, table)
    paste_sql(table, ".", var)
  }
  else {
    var
  }
}

sql_escape_ident <- function(con, x) {
  UseMethod("sql_escape_ident")
}

#' @export
sql_escape_ident.DBIConnection <- function(con, x) {
  DBI::dbQuoteIdentifier(con, x)
}

#' @export
sql_escape_ident.TestConnection <- function(con, x) {
  dbplyr::sql_quote(x, "`")
}

cache <- function() {
  if (!is_attached("dbtools_cache")) {
    get("attach")(new_environment(), name = "dbtools_cache", pos = length(search()) - 1)
  }
  search_env("dbtools_cache")
}

cache_computation <- function(name, computation) {
  cache <- cache()

  if (env_has(cache, name)) {
    env_get(cache, name)
  } else {
    res <- force(computation)
    env_poke(cache, name, res)
    res
  }
}

#' Create a database table in temporary in-memory database
#'
#' `memdb_frame2()` works like [`dbplyr::memdb_frame()`]: it creates a table in
#' an in-memory SQLite database.
#'
#' @export
con_memdb <- function() {
  cache_computation(
    "con_memdb",
    DBI::dbConnect(RSQLite::SQLite(), ":memory:", create = TRUE)
  )
}

#' @param ... Passed to [`tibble::tibble()`].
#' @param .name Name of table in database.
#'
#' @export
#' @rdname con_memdb
memdb_frame2 <- function(..., .name) {
  con_frame(..., .name = .name, .con = con_memdb())
}

con_frame <- function(..., .name, .con) {
  DBI::dbWriteTable(
    conn = .con,
    name = .name,
    value = tibble::tibble(...),
    temporary = TRUE,
    overwrite = TRUE,
    row.names = FALSE
  )
}
# nocov end
