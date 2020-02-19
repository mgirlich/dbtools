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

prepare_table <- function(value = df) {
  if (DBI::dbExistsTable(con, test_table)) {
    DBI::dbRemoveTable(con, test_table)
  }

  table <- DBI::dbQuoteIdentifier(con, test_table)

  fields <- vapply(value, function(x) DBI::dbDataType(con, x), character(1))
  field_names <- DBI::dbQuoteIdentifier(con, names(fields))
  field_types <- unname(fields)
  fields <- SQL(paste0(field_names, " ", field_types))

  lcl_exec("
    CREATE TABLE {`table`} (
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
