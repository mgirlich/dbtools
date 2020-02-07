#' sql_set(c(a = "a old", b = "b old"), con = con)
#' sql_set(c(a = "a old", b = "b old"), con = con, cols = c("x", "y"))
#' sql_set(list(a = "a old", b = sql('"table"."old"')), con = con)
sql_set <- function(values, con, cols = names2(values)) {
  # TODO check type of values?
  stopifnot(is.character(cols))
  if (any(cols == "")) {
    abort("invalid names")
  }

  if (vec_duplicate_any(cols)) {
    abort("duplicate names")
  }

  if (length(cols) != length(values)) {
    abort("cols must have same length as values")
  }

  glue_sql("{`cols`} = {`values`}", .con = con)
}

#' sql_select(c(a = "a old", "b old"), con = con)
#' sql_select(list(a = "a old", b = sql('"table"."old"')), con = con)
sql_select <- function(values, con, cols = names2(values)) {
  ifelse(
    cols != "",
    glue_sql("{`values`} AS {`cols`}", .con = con),
    glue_sql("{`values`}", .con = con)
  ) %>%
    sql()
}



assign_global <- function(...) {
  dots <- list(...)
  for (i in seq_along(dots)) {
    assign(names(dots)[[i]], dots[[i]], envir = global_env())
  }
}

debugonce(batch_wise_db)
db_update_data(
  mtcars %>%
    tibble::rownames_to_column("name") %>%
    dplyr::mutate(mpg2 = mpg + 100) %>%
    dplyr::slice(1:3),
  table = "mr_utils_test",
  con = con,
  update = c("mpg" = "mpg2"),
  where = "name",
  returning = c("name", "mpg"),
  batch_size = 1
)

db_update_data(
  mtcars %>%
    tibble::rownames_to_column("name") %>%
    dplyr::mutate(mpg2 = mpg + 100) %>%
    dplyr::slice(1:3),
  table = "mr_utils_test",
  con = con,
  update = c("mpg" = "mpg2"),
  where = "name",
  batch_size = 1
)

con <- mr.amsutils::db_con_ams()
debugonce(sql_update)
debugonce(sql_clause_generator)
sql_update(
  mtcars %>%
    tibble::rownames_to_column("name") %>%
    dplyr::transmute(
      name,
      mpg,
      mpg2 = mpg + 100
    ) %>%
    dplyr::slice(1),
  table = "mr_utils_test",
  con = mr.amsutils::db_con_ams(),
  update = c("mpg" = "mpg2"),
  where = "name",
  returning = list("name", sql("source.mpg2"))
)
