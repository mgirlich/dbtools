con_pg <- function() {
  cache_computation(
    "con_pg",
    DBI::dbConnect(RPostgres::Postgres(), dbname = "postgres")
  )
}

pg_frame2 <- function(..., .name) {
  con_frame(..., .name = .name, .con = con_pg())
}

has_pg <- function() {
  is_null(purrr::safely(con_pg)()$error)
}



df <- data.frame(
  id1 = c(1, 1, 2, 3),
  id2 = c("a", "b", "b", "c"),
  value1 = c(10, 20, 11, 11),
  value2 = c("A", "AA", "B", "B"),
  stringsAsFactors = FALSE
)

test_table <- "dbtools_test"
columns <- c("id1", "id2")
index_name <- paste(c(test_table, columns), collapse = "_")

prepare_table <- function(value = df, con = con_memdb()) {
  if (DBI::dbExistsTable(con, test_table)) {
    DBI::dbRemoveTable(con, test_table)
  }

  fields <- vapply(value, function(x) DBI::dbDataType(con, x), character(1))
  field_names <- DBI::dbQuoteIdentifier(con, names(fields))
  field_types <- unname(fields)
  fields <- DBI::SQL(paste0(field_names, " ", field_types))

  lcl_exec("
    CREATE TABLE {`test_table`} (
      {fields*},
      UNIQUE({`columns`*})
    );")

  DBI::dbAppendTable(
    conn = con,
    name = test_table,
    value = value
  )

  if (is_postgres(con)) {
    lcl_exec("
      ALTER TABLE {`test_table`}
      ADD CONSTRAINT {`index_name`}
      UNIQUE ({`columns`*});")
  }
}
