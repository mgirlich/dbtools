# tmp <- tempfile()
con <- DBI::dbConnect(RSQLite::SQLite(), tempfile())
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

mtcars_df <- tibble::rownames_to_column(mtcars)[, 1:4]

lcl_exec <- function(...) {
  sql <- glue::glue_sql(..., .con = con_memdb(), .envir = parent.frame())
  DBI::dbExecute(conn = con_memdb(), sql)
}



get_tbl <- function() {
  DBI::dbReadTable(con_memdb(), test_table, row.names = FALSE)
}

ref_file <- function(...) {
  base <- ref_dir()
  if (!dir.exists(base)) {
    dir.create(base)
  }
  file.path(base, ...)
}

ref_dir <- function() {
  test_path("references")
}


def_returning <- list(
  "name",
  id = "id2",
  time = sql("now()")
)
def_update <- list(
  "name",
  id = "id2"
)
def_where <- list(
  "name",
  col2 = "id",
  sql("now()")
)
