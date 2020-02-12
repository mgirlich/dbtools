df <- tibble::tribble(
  ~ id1, ~ id2, ~ value1, ~ value2,
      1,   "a",       10,       "A",
      1,   "b",       20,       "AA",
      2,   "b",       11,       "B",
      3,   "c",       11,       "B",
)

df2 <- tibble::tribble(
  ~ id1, ~ id2, ~ value1, ~ value2,
      1,   "b",       20,       "A",
      2,   "b",       21,       "B"
)



df <- as.data.frame(df)

test_table <- "dbtools_test"
columns <- c("id1", "id2")
index_name <- paste(c(test_table, columns), collapse = "_")

prepare_table <- function(value = df) {
  lcl_exec("DROP TABLE IF EXISTS {`test_table`}")

  DBI::dbWriteTable(
    conn = con,
    name = test_table,
    value = value
  )

  lcl_exec('
    ALTER TABLE {`test_table`}
    ADD CONSTRAINT {`index_name`}
    UNIQUE ({`columns`*});')
}
