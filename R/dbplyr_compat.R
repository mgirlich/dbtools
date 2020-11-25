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

sql_escape_string <- function(con, x) {
  UseMethod("sql_escape_string")
}

#' @export
sql_escape_string.DBIConnection <- function(con, x) {
  DBI::dbQuoteString(con, x)
}

#' @export
sql_escape_string.TestConnection <- function(con, x) {
  dbplyr::sql_quote(x, "'")
}

sql_escape_logical <- function(con, x) {
  UseMethod("sql_escape_logical")
}

#' @export
sql_escape_logical.ACCESS <- function(con, x) {
  y <- ifelse(x, -1, 0)
  y[is.na(x)] <- "NULL"
  y
}

#' @export
sql_escape_logical.DBIConnection <- function(con, x) {
  y <- as.character(x)
  y[is.na(x)] <- "NULL"
  y
}

#' @export
`sql_escape_logical.Microsoft SQL Server` <- function(con, x) {
  # if (mssql_needs_bit()) {
  if (TRUE) {
    y <- ifelse(x, "1", "0")
  } else {
    y <- as.character(x)
  }
  y[is.na(x)] <- "NULL"
  y
}

#' @export
sql_escape_logical.SQLiteConnection <- function(con, x) {
  y <- as.character(as.integer(x))
  y[is.na(x)] <- "NULL"
  y
}

sql_escape_raw <- function(con, x) {
  UseMethod("sql_escape_raw")
}

#' @export
sql_escape_raw.DBIConnection <- function(con, x) {
  paste0(c("X'", format(x), "'"), collapse = "")
}

#' @export
`sql_escape_raw.Microsoft SQL Server` <- function(con, x) {
  paste0(c("0x", format(x)), collapse = "")
}

sql_escape_date <- dbplyr::sql_escape_date
sql_escape_datetime <- dbplyr::sql_escape_datetime
sql_escape_logical <- dbplyr::sql_escape_logical
sql_escape_raw <- dbplyr::sql_escape_raw

# escape ------------------------------------------------------------------

# this is a non-collapsing version of `dbplyr::escape`

escape_value <- function(x, parens = NA, collapse = " ", con = NULL) {
  if (is.null(con)) {
    stop("`con` must not be NULL", call. = FALSE)
  }

  UseMethod("escape_value")
}

#' @export
escape_value.ident <- function(x, parens = FALSE, collapse = ", ", con = NULL) {
  # y <- sql_escape_ident(con, x)
  # sql_vector(names_to_as(y, names2(x), con = con), parens, collapse, con = con)
  sql_escape_ident(con, x)
}

#' @export
escape_value.logical <- function(x, parens = NA, collapse = ", ", con = NULL) {
  # sql_vector(sql_escape_logical(con, x), parens, collapse, con = con)
  sql_escape_logical(con, x)
}

#' @export
escape_value.factor <- function(x, parens = NA, collapse = ", ", con = NULL) {
  # x <- as.character(x)
  # escape.character(x, parens = parens, collapse = collapse, con = con)
  x <- as.character(x)
  escape_value.character(x, parens = parens, collapse = collapse, con = con)
}

#' @export
escape_value.Date <- function(x, parens = NA, collapse = ", ", con = NULL) {
  # sql_vector(sql_escape_date(con, x), parens, collapse, con = con)
  sql_escape_date(con, x)
}

#' @export
escape_value.POSIXt <- function(x, parens = NA, collapse = ", ", con = NULL) {
  # sql_vector(sql_escape_datetime(con, x), parens, collapse, con = con)
  sql_escape_datetime(con, x)
}

#' @export
escape_value.character <- function(x, parens = NA, collapse = ", ", con = NULL) {
  # sql_vector(sql_escape_string(con, x), parens, collapse, con = con)
  sql_escape_string(con, x)
}

#' @export
escape_value.double <- function(x, parens = NA, collapse = ", ", con = NULL) {
  out <- ifelse(is_wholenumber(x), sprintf("%.1f", x), as.character(x))

  # Special values
  out[is.na(x)] <- "NULL"
  inf <- is.infinite(x)
  out[inf & x > 0] <- "'Infinity'"
  out[inf & x < 0] <- "'-Infinity'"

  # sql_vector(out, parens, collapse, con = con)
  out
}

is_wholenumber <- function(x) {
  trunc(x) == x
}

#' @export
escape_value.integer <- function(x, parens = NA, collapse = ", ", con = NULL) {
  x[is.na(x)] <- "NULL"
  # sql_vector(x, parens, collapse, con = con)
  x
}

#' @export
escape_value.integer64 <- function(x, parens = NA, collapse = ", ", con = NULL) {
  x <- as.character(x)
  x[is.na(x)] <- "NULL"
  # sql_vector(x, parens, collapse, con = con)
  x
}

#' @export
escape_value.blob <- function(x, parens = NA, collapse = ", ", con = NULL) {
  pieces <- vapply(x, sql_escape_raw, character(1), con = con)
  # sql_vector(pieces, isTRUE(parens) || length(pieces) > 1, collapse, con = con)
  pieces
}

#' @export
escape_value.NULL <- function(x, parens = NA, collapse = " ", con = NULL) {
  sql("NULL")
}

#' @export
escape_value.sql <- function(x, parens = NULL, collapse = NULL, con = NULL) {
  # sql_vector(x, isTRUE(parens), collapse, con = con)
  # escape_value(as.character(x), con = con)
  x
}

#' @export
escape_value.list <- function(x, parens = TRUE, collapse = ", ", con = NULL) {
  pieces <- vapply(x, escape_value, character(1), con = con)
  # sql_vector(pieces, parens, collapse, con = con)
  pieces
}

#' @export
escape_value.data.frame <- function(x, parens = TRUE, collapse = ", ", con = NULL) {
  error_embed("a data.frame", "df$x")
}

#' @export
escape_value.reactivevalues <- function(x, parens = TRUE, collapse = ", ", con = NULL) {
  error_embed("shiny inputs", "inputs$x")
}

# Also used in default_ops() for reactives
error_embed <- function(type, expr) {
  abort(c(
    glue("Cannot translate {type} to SQL."),
    glue("Force evaluation in R with (e.g.) `!!{expr}` or `local({expr})`")
  ))
}

# connection helpers ------------------------------------------------------

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
