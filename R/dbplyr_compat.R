# nocov start
sql_table_prefix <- function (con, var, table = NULL) {
  var <- sql_escape_ident(con, var)
  if (!is.null(table)) {
    table <- sql_escape_ident(con, table)
    paste_sql(table, ".", var)
  }
  else {
    var
  }
}

sql_escape_ident <- function (con, x) {
    UseMethod("sql_escape_ident")
}

sql_escape_ident.DBIConnection <- function (con, x) {
    DBI::dbQuoteIdentifier(con, x)
}

sql_escape_ident.TestConnection <- function (con, x) {
    sql_quote(x, "`")
}
# nocov end
