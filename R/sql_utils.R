paste_sql <- function(..., sep = "", collapse = NULL) {
  SQL(paste(..., sep = sep, collapse = collapse))
}

collapse_sql <- function(x, collapse) {
  paste_sql(x, collapse = collapse)
}

is_sql <- function(x) {
  inherits(x, "SQL") || inherits(x, "sql")
}

are_sql <- function(x) {
  purrr::map_lgl(x, is_sql)
}

is_scalar_sql <- function(x) {
  is_sql(x) && (length(x) == 1)
}
