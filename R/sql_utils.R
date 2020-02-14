paste_sql <- function(..., sep = "", collapse = NULL) {
  SQL(paste(..., sep = sep, collapse = collapse))
}

collapse_sql <- function(x, collapse) {
  paste_sql(x, collapse = collapse)
}

is_sql <- function(x) {
  inherits(x, "SQL") || inherits(x, "sql")
}

is_scalar_sql <- function(x) {
  is_sql(x) && (length(x) == 1)
}

is_sql_star <- function(x) {
  identical(x, SQL("*")) && is_scalar_sql(x)
}

is_sql_chr_list <- function(x, chr_names, sql_names) {
  has_valid_name <- function(name, name_flag) {
    if (is_true(name_flag)) {
      name != ""
    } else if (is_false(name_flag)) {
      name == ""
    } else {
      TRUE
    }
  }

  is_valid_element <- function(elt, elt_name) {
    valid_chr_flag <- is_bare_character(elt, n = 1) && has_valid_name(elt_name, chr_names)
    valid_sql_flag <- is_scalar_sql(elt) && (
      has_valid_name(elt_name, sql_names) ||
        is_sql_star(elt)
    )

    valid_chr_flag || valid_sql_flag
  }

  all(purrr::map2_lgl(x, names2(x), is_valid_element))
}
