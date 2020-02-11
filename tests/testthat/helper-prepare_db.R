df <- tibble::tribble(
  ~ id1, ~ id2, ~ value1, ~ value2,
      1,   "a",       10,       "A",
      2,   "b",       11,       "B",

      1,   "b",       20,       "A",
      2,   "b",       21,       "B"
)
df <- as.data.frame(df)

test_table <- "dbtools_test"
columns <- c("id1", "id2")
index_name <- paste(c(test_table, columns), collapse = "_")
df_db <- df[1:2, ]

prepare_table <- function(value = df_db) {
  lcl_exec("DROP TABLE IF EXISTS {`test_table`}")

  DBI::dbWriteTable(
    conn = con,
    name = test_table,
    value = value
  )

  # lcl_exec('CREATE UNIQUE INDEX {`index_name`} ON {`test_table`} ({`columns`*})')
  # lcl_exec('CREATE UNIQUE INDEX {`index_name`} ON {`test_table`} ({`columns`*})')

  lcl_exec('
    ALTER TABLE {`test_table`}
    ADD CONSTRAINT {`index_name`}
    UNIQUE ({`columns`*});')
}
