paste_sql <- function(..., sep = "", collapse = NULL) {
  sql(paste(..., sep = sep, collapse = collapse))
}

collapse_sql <- function(x, collapse) {
  paste_sql(x, collapse = collapse)
}

is_sql <- function(x) {
  inherits(x, "sql")
}

are_sql <- function(x) {
  purrr::map_lgl(x, is_sql)
}

is_scalar_sql <- function(x) {
  is_sql(x) && (length(x) == 1)
}

maybe_ident <- function(x) {
  UseMethod("maybe_ident")
}

#' @export
maybe_ident.character <- function(x) {
  ident(x)
}

#' @export
maybe_ident.ident <- function(x) {
  x
}

#' @export
maybe_ident.sql <- function(x) {
  x # nocov
}
