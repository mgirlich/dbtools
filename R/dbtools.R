#' @import vctrs
#' @import rlang
#' @import DBI
"_PACKAGE"

#' @importFrom glue glue_sql
#' @export
glue::glue_sql

#' @importFrom DBI SQL
#' @export
DBI::SQL

# nocov start
#' @importClassesFrom RPostgres PqConnection
#' @importMethodsFrom DBI dbQuoteIdentifier
setMethod("dbQuoteIdentifier", c("PqConnection", "sql"), function(conn, x, ...) {
  x
})

#' @importClassesFrom RSQLite SQLiteConnection
setMethod("dbQuoteIdentifier", c("SQLiteConnection", "sql"), function(conn, x, ...) {
  x
})
# nocov end
