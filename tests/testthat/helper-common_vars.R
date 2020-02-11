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
