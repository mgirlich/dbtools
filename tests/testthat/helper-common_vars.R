con <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  dbname = "postgres"
)

lcl_exec <- function(...) {
  DBI::dbExecute(conn = con, glue::glue_sql(..., .con = con))
}



get_tbl <- function() {
  DBI::dbReadTable(con, test_table, row.names = FALSE)
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
  SQL("now()")
)
