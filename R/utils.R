#' Auto name a list or character vector
#'
#' @noRd
#' @examples
#' \dontrun{
#' auto_name(list("body"))
#' list("body", left = "right", SQL("unnamed_sql"), left = SQL("named_sql")) %>%
#'   auto_name()
#' }
auto_name <- function(x) {
  # only auto name character
  chr_flag <- purrr::map_lgl(x, is_bare_character)
  nms_flag <- names2(x) != ""
  update_flag <- !nms_flag & chr_flag

  auto_names <- purrr::map_chr(x[update_flag], as_name)
  names(x)[update_flag] <- auto_names
  x
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
